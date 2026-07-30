import { spawn } from 'child_process';
import JSONbig from 'json-bigint';
import { Cardano } from '@cardano-sdk/core';
import { logger } from '../utils/logging';
import { NetworkMagics } from '../../common/types/cardano-node.types';
import {
  GovernanceQueryErrorType,
  DRepListQueryPayload,
  DRepStakeQueryPayload,
  DRepDirectoryEntry,
  DRepStatus,
  DrepActivity,
} from '../../common/types/governance.types';

// Use json-bigint with storeAsString: true so all large numbers become strings.
// This preserves lovelace precision beyond Number.MAX_SAFE_INTEGER.
const JSONBig = JSONbig({ storeAsString: true });

/**
 * Error subclass thrown by GovernanceQueryService.
 * Carries a typed error code for the renderer to handle gracefully.
 */
export class GovernanceQueryError extends Error {
  readonly queryErrorType: GovernanceQueryErrorType;
  readonly details: string | undefined;

  constructor(
    type: GovernanceQueryErrorType,
    message: string,
    details?: string
  ) {
    super(message);
    this.name = 'GovernanceQueryError';
    this.queryErrorType = type;
    this.details = details;
  }
}

/**
 * Singleton service that queries the local cardano-node for DRep ledger state
 * using `cardano-cli`. Runs in the Electron main process only.
 *
 * All lovelace values are parsed with json-bigint (storeAsString) and kept as
 * decimal strings. The renderer rehydrates them to BigNumber.
 *
 * In-flight requests are deduplicated. Last-successful data is retained for
 * stale-while-refresh continuity in later slices.
 */
export class GovernanceQueryService {
  private static instance: GovernanceQueryService | null = null;

  /**
   * Per-phase CLI timeout budgets (ms) per the shared-design-tokens two-phase
   * refresh contract. The 30s stake budget is provisional until real
   * synced-node latency is measured.
   */
  private static readonly REGISTRATION_TIMEOUT_MS = 10_000;
  private static readonly STAKE_TIMEOUT_MS = 30_000;

  /**
   * Structural signature of an optparse-applicative argv rejection (bad era
   * token, invalid flag, missing required argument). Node-side query failures
   * never print it, so it gates the conway era fallback safely.
   */
  private static readonly CLI_USAGE_SIGNATURE =
    /(invalid (option|argument)|missing:|usage:)/i;

  private cliBin = 'cardano-cli';
  private nodeSocketPath: string | null = null;
  private isSelfnode = false;
  /**
   * cardano-cli network flag string (e.g. '--mainnet' or '--testnet-magic 1').
   * Derived exclusively from the node config via setNetwork() — never from
   * renderer/IPC input. Null until setNetwork() runs for a known cluster.
   */
  private networkFlag: string | null = null;
  private lastSuccessfulData: DRepListQueryPayload | null = null;
  private inFlightRegistrations: Promise<DRepListQueryPayload> | null = null;
  private inFlightStake: Promise<DRepStakeQueryPayload> | null = null;

  private constructor() {
    // singleton — use getInstance()
  }

  static getInstance(): GovernanceQueryService {
    if (!GovernanceQueryService.instance) {
      GovernanceQueryService.instance = new GovernanceQueryService();
    }
    return GovernanceQueryService.instance;
  }

  /** Set the path to the cardano-cli binary. */
  setCliBin(binPath: string): void {
    this.cliBin = binPath;
  }

  /**
   * Set the cardano-node Unix socket path.
   * Called by CardanoNode after node.start() resolves.
   */
  setNodeSocketPath(socketPath: string | null): void {
    this.nodeSocketPath = socketPath;
  }

  /**
   * Set whether Daedalus is running in selfnode mode.
   * In selfnode mode CLI-based DRep queries are unsupported.
   * Called by CardanoNode during start and reset.
   */
  setSelfnodeMode(mode: boolean): void {
    this.isSelfnode = mode;
  }

  /**
   * Derive the cardano-cli network flag from the active node cluster.
   * Called by CardanoNode during start. The flag is required by every
   * `cardano-cli ... query ...` invocation (`--mainnet` or
   * `--testnet-magic <N>`); without it the CLI fails with
   * "Missing: (--mainnet | --testnet-magic NATURAL)".
   *
   * The flag is derived solely from node config — never from renderer/IPC input.
   *
   * @param cluster The node cluster string from CardanoNode._config.cluster.
   */
  setNetwork(cluster: string): void {
    if (cluster === 'mainnet' || cluster === 'mainnet_flight') {
      this.networkFlag = '--mainnet';
    } else if (cluster === 'development') {
      // development cluster uses magic 42 (not present in NetworkMagics)
      this.networkFlag = '--testnet-magic 42';
    } else {
      const magic = NetworkMagics[cluster]?.[0];
      if (magic != null) {
        this.networkFlag = `--testnet-magic ${magic}`;
      } else {
        // Unknown cluster — queries will fail with a clear error in _runCliQuery.
        this.networkFlag = null;
      }
    }
  }

  /**
   * Clear all persisted state: last successful data, in-flight refresh promise,
   * socket path, and selfnode mode. Called by CardanoNode._reset() on stop/crash.
   */
  reset(): void {
    this.lastSuccessfulData = null;
    this.inFlightRegistrations = null;
    this.inFlightStake = null;
    this.nodeSocketPath = null;
    this.isSelfnode = false;
    this.networkFlag = null;
  }

  /**
   * Phase 1: fetch DRep registrations (no stake read) from the local node.
   * Voting power is always null here; fetchDRepStake() enriches it.
   * Deduplicates in-flight requests — if a refresh is already running,
   * the same promise is returned to all concurrent callers.
   *
   * @throws {GovernanceQueryError} on socket-unavailable, CLI-not-found,
   *         query-failed, parse-failed, or timeout.
   */
  async fetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    if (this.inFlightRegistrations) {
      return this.inFlightRegistrations;
    }

    this.inFlightRegistrations = this._doFetchDRepRegistrations();

    try {
      const result = await this.inFlightRegistrations;
      this.lastSuccessfulData = result;
      return result;
    } finally {
      this.inFlightRegistrations = null;
    }
  }

  /**
   * Phase 2: fetch the DRep stake distribution keyed by the same CIP-129
   * DRep id the registration payload derives, so the renderer merges by
   * plain string equality.
   *
   * @throws {GovernanceQueryError} on the same failure classes as Phase 1.
   */
  async fetchDRepStake(): Promise<DRepStakeQueryPayload> {
    if (this.inFlightStake) {
      return this.inFlightStake;
    }

    this.inFlightStake = this._doFetchDRepStake();

    try {
      return await this.inFlightStake;
    } finally {
      this.inFlightStake = null;
    }
  }

  /** Return the last successful query result, or null if none exists. */
  getLastSuccessfulData(): DRepListQueryPayload | null {
    return this.lastSuccessfulData;
  }

  // ---- Private Implementation ----

  private _assertQueryable(): void {
    if (this.isSelfnode) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.SelfnodeCliUnsupported,
        'DRep data is unavailable in selfnode mode. A synced node is required.'
      );
    }

    if (!this.nodeSocketPath) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.SocketUnavailable,
        'Cardano node socket path is not available. The node may not be fully started.'
      );
    }
  }

  private async _doFetchDRepRegistrations(): Promise<DRepListQueryPayload> {
    this._assertQueryable();

    try {
      const [drepStateStdout, tipStdout] = await Promise.all([
        this._runCliQueryWithEraFallback(
          ['query', 'drep-state', '--all-dreps', '--output-json'],
          GovernanceQueryService.REGISTRATION_TIMEOUT_MS
        ),
        this._runCliQueryWithEraFallback(
          ['query', 'tip', '--output-json'],
          GovernanceQueryService.REGISTRATION_TIMEOUT_MS
        ),
      ]);

      const currentEpoch = this._parseTipEpoch(tipStdout);
      const dreps = this._parseDRepState(drepStateStdout, currentEpoch);

      return {
        dreps,
        fetchedAt: Date.now(),
        epoch: currentEpoch,
      };
    } catch (error) {
      if (error instanceof GovernanceQueryError) {
        throw error;
      }
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.QueryFailed,
        `DRep query failed: ${
          error instanceof Error ? error.message : String(error)
        }`
      );
    }
  }

  private async _doFetchDRepStake(): Promise<DRepStakeQueryPayload> {
    this._assertQueryable();

    try {
      const stakeStdout = await this._runCliQueryWithEraFallback(
        ['query', 'drep-stake-distribution', '--all-dreps', '--output-json'],
        GovernanceQueryService.STAKE_TIMEOUT_MS
      );

      return {
        stakeByDRepId: this._parseStakeDistribution(stakeStdout),
        fetchedAt: Date.now(),
      };
    } catch (error) {
      if (error instanceof GovernanceQueryError) {
        throw error;
      }
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.QueryFailed,
        `DRep stake query failed: ${
          error instanceof Error ? error.message : String(error)
        }`
      );
    }
  }

  /**
   * Run a governance query with the preferred `latest` era flag.
   * Falls back to `conway` only when the installed cardano-cli rejects `latest`.
   */
  private async _runCliQueryWithEraFallback(
    args: string[],
    timeoutMs: number
  ): Promise<string> {
    try {
      return await this._runCliQuery(['latest', ...args], timeoutMs);
    } catch (error) {
      if (this._shouldRetryWithConway(error)) {
        logger.warn(
          'GovernanceQueryService: retrying governance query with conway era flag',
          { args }
        );
        return this._runCliQuery(['conway', ...args], timeoutMs);
      }
      throw error;
    }
  }

  private _shouldRetryWithConway(error: unknown): boolean {
    return (
      error instanceof GovernanceQueryError &&
      error.queryErrorType === GovernanceQueryErrorType.UsageError
    );
  }

  /**
   * Spawn cardano-cli to query the local node.
   * Sets CARDANO_NODE_SOCKET_PATH in the child process environment — never as
   * a user-controllable argv flag.
   */
  private _runCliQuery(args: string[], timeoutMs: number): Promise<string> {
    return new Promise((resolve, reject) => {
      if (this.networkFlag === null) {
        reject(
          new GovernanceQueryError(
            GovernanceQueryErrorType.QueryFailed,
            'Cardano network is not set. The node cluster must be configured before querying DRep data.'
          )
        );
        return;
      }

      // The network flag (--mainnet / --testnet-magic <N>) is a per-subcommand
      // option of `query tip` / `query drep-state`, so it must be appended
      // AFTER the subcommand args — not before the era token. cardano-cli's
      // top-level parser has no --mainnet/--testnet-magic option, so prepending
      // it is rejected with "Invalid option". Final argv:
      //   ['latest', 'query', 'tip', '--output-json', '--mainnet']
      // or ['latest', 'query', 'drep-state', ..., '--testnet-magic', '1'].
      const flagTokens = this.networkFlag.split(' ');
      const fullArgs = [...args, ...flagTokens];

      const env: Record<string, string | undefined> = {};
      // Copy only string-valued process.env entries
      Object.keys(process.env).forEach((key) => {
        const val = process.env[key];
        if (typeof val === 'string') {
          env[key] = val;
        }
      });
      env.CARDANO_NODE_SOCKET_PATH = this.nodeSocketPath!;

      const child = spawn(this.cliBin, fullArgs, {
        env: env as typeof process.env,
        stdio: ['ignore', 'pipe', 'pipe'],
      });

      let stdout = '';
      let stderr = '';
      let timeout: ReturnType<typeof setTimeout> | null = null;

      // With stdio ['ignore', 'pipe', 'pipe'], stdout and stderr are readable streams.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion
      (child.stdout as NodeJS.ReadableStream).on('data', (data: Buffer) => {
        stdout += data.toString('utf-8');
      });

      // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion
      (child.stderr as NodeJS.ReadableStream).on('data', (data: Buffer) => {
        stderr += data.toString('utf-8');
      });

      child.on('error', (err) => {
        if (timeout) clearTimeout(timeout);
        reject(
          new GovernanceQueryError(
            GovernanceQueryErrorType.CliNotFound,
            `cardano-cli binary not found at "${this.cliBin}": ${err.message}`
          )
        );
      });

      timeout = setTimeout(() => {
        child.kill('SIGTERM');
        reject(
          new GovernanceQueryError(
            GovernanceQueryErrorType.Timeout,
            `cardano-cli DRep query timed out after ${timeoutMs}ms`
          )
        );
      }, timeoutMs);

      child.on('close', (code) => {
        if (timeout) clearTimeout(timeout);
        if (code !== 0) {
          const trimmedStderr = stderr.trim();
          const isUsageRejection =
            GovernanceQueryService.CLI_USAGE_SIGNATURE.test(trimmedStderr);
          reject(
            new GovernanceQueryError(
              isUsageRejection
                ? GovernanceQueryErrorType.UsageError
                : GovernanceQueryErrorType.QueryFailed,
              `cardano-cli exited with code ${code}`,
              trimmedStderr || undefined
            )
          );
          return;
        }
        resolve(stdout);
      });
    });
  }

  /**
   * Parse the current epoch from `cardano-cli latest query tip --output-json`.
   * The tip object has shape: { "epoch": number, "hash": "...", "slot": number, ... }.
   * Throws ParseFailed when epoch is absent or unparseable.
   */
  private _parseTipEpoch(rawOutput: string): number {
    let parsed: unknown;
    try {
      parsed = JSONBig.parse(rawOutput);
    } catch (err) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        'Failed to parse query tip JSON output',
        err instanceof Error ? err.message : undefined
      );
    }

    if (!parsed || typeof parsed !== 'object' || !('epoch' in parsed)) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        'query tip output is missing the required epoch field'
      );
    }

    return this._parseRequiredEpochValue(
      (parsed as Record<string, unknown>).epoch,
      'query tip output contains a non-numeric epoch field'
    );
  }

  /**
   * Parse the raw JSON stdout from `cardano-cli latest query drep-state --all-dreps --output-json`.
   *
   * The CLI output is an array of tuples: `[[credential, state], ...]` where:
   * - `credential` is `{ keyHash: "hex" }` or `{ scriptHash: "hex" }`
   * - `state` has `expiry` (epoch number), `anchor` (object|null), `deposit` (lovelace).
   *
   * DRep IDs are derived from credentials using `Cardano.DRepID.cip129FromCredential`.
   * Status is conservatively derived from `expiry` vs `currentEpoch`.
   * Voting power is always null in this phase; the stake phase enriches it.
   *
   * A parse failure on any entry throws ParseFailed so the renderer
   * never renders partial/corrupt data.
   */
  private _parseDRepState(
    rawOutput: string,
    currentEpoch: number
  ): DRepDirectoryEntry[] {
    let parsed: unknown;
    try {
      parsed = JSONBig.parse(rawOutput);
    } catch (err) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        'Failed to parse CLI JSON output',
        err instanceof Error ? err.message : undefined
      );
    }

    if (!Array.isArray(parsed)) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `Expected an array of tuples from drep-state query, got ${typeof parsed}`
      );
    }

    return (parsed as Array<unknown>).map((tuple: unknown, index: number) => {
      try {
        if (!Array.isArray(tuple) || tuple.length < 2) {
          throw new GovernanceQueryError(
            GovernanceQueryErrorType.ParseFailed,
            `DRep entry at index ${index} is not a [credential, state] tuple`
          );
        }

        const credential = tuple[0] as Record<string, unknown>;
        const state = tuple[1] as Record<string, unknown>;

        const drepId = this._credentialToDRepId(credential, index);

        const expiryRaw = state.expiry;
        if (expiryRaw === undefined || expiryRaw === null) {
          throw new GovernanceQueryError(
            GovernanceQueryErrorType.ParseFailed,
            `DRep entry at index ${index} is missing the required expiry field`
          );
        }
        const expiry = this._parseRequiredEpochValue(
          expiryRaw,
          `DRep entry at index ${index} has non-numeric expiry`
        );

        // Status: conservative — only active/inactive from expiry vs currentEpoch
        const status: DRepStatus =
          expiry <= currentEpoch ? 'inactive' : 'active';

        // drepActivity: remaining epochs until expiry; 0 when inactive
        const drepActivity: DrepActivity = Math.max(0, expiry - currentEpoch);

        // Phase 1 never reads stake; fetchDRepStake() fills voting power.
        const votingPower: string | null = null;

        const anchor = this._parseAnchor(state, index);

        // The bulk drep-state query never fetches an anchor; the verified name
        // is filled in the renderer from the per-DRep anchor channel.
        return {
          drepId,
          votingPower,
          status,
          drepActivity,
          anchor,
          verifiedName: null,
        };
      } catch (err) {
        if (err instanceof GovernanceQueryError) {
          throw err;
        }
        logger.error('GovernanceQueryService: failed to parse DRep entry', {
          index,
          error: err,
        });
        throw new GovernanceQueryError(
          GovernanceQueryErrorType.ParseFailed,
          `Failed to parse DRep entry at index ${index}`,
          err instanceof Error ? err.message : undefined
        );
      }
    });
  }

  /**
   * Parse `drep-stake-distribution --all-dreps --output-json` into a
   * CIP-129-keyed decimal-string lovelace map.
   *
   * cardano-cli serialized this query as an object map in some major versions
   * and as an array of [key, value] pairs in others; both container shapes are
   * accepted. Keys are `drep-keyHash-<hex>` / `drep-scriptHash-<hex>` plus the
   * two voting sentinels, which are skipped (sentinels are ballot forms, never
   * directory entries). Any other key or value shape throws ParseFailed.
   * Error messages identify entries by index only — never by key or id.
   */
  private _parseStakeDistribution(rawOutput: string): Record<string, string> {
    let parsed: unknown;
    try {
      parsed = JSONBig.parse(rawOutput);
    } catch (err) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        'Failed to parse drep-stake-distribution JSON output',
        err instanceof Error ? err.message : undefined
      );
    }

    let pairs: Array<[string, unknown]>;
    if (Array.isArray(parsed)) {
      pairs = (parsed as Array<unknown>).map((entry, index) => {
        if (
          !Array.isArray(entry) ||
          entry.length < 2 ||
          typeof entry[0] !== 'string'
        ) {
          throw new GovernanceQueryError(
            GovernanceQueryErrorType.ParseFailed,
            `Stake entry at index ${index} is not a [key, value] pair`
          );
        }
        return [entry[0], entry[1]] as [string, unknown];
      });
    } else if (parsed && typeof parsed === 'object') {
      pairs = Object.entries(parsed as Record<string, unknown>);
    } else {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `Expected an object map or array of pairs from drep-stake-distribution, got ${typeof parsed}`
      );
    }

    const stakeByDRepId: Record<string, string> = {};
    pairs.forEach(([key, value], index) => {
      if (key === 'drep-alwaysAbstain' || key === 'drep-alwaysNoConfidence') {
        return;
      }

      const keyHashMatch = /^drep-keyHash-([0-9a-fA-F]+)$/.exec(key);
      const scriptHashMatch = /^drep-scriptHash-([0-9a-fA-F]+)$/.exec(key);
      if (!keyHashMatch && !scriptHashMatch) {
        throw new GovernanceQueryError(
          GovernanceQueryErrorType.ParseFailed,
          `Stake entry at index ${index} has an unknown key shape`
        );
      }

      const stakeString = String(value);
      if (
        (typeof value !== 'string' && typeof value !== 'number') ||
        !/^\d+$/.test(stakeString)
      ) {
        throw new GovernanceQueryError(
          GovernanceQueryErrorType.ParseFailed,
          `Stake entry at index ${index} has a non-numeric stake value`
        );
      }

      const drepId = keyHashMatch
        ? this._credentialToDRepId({ keyHash: keyHashMatch[1] }, index)
        : this._credentialToDRepId({ scriptHash: scriptHashMatch![1] }, index);

      stakeByDRepId[drepId] = stakeString;
    });

    return stakeByDRepId;
  }

  /**
   * Derive a CIP-129 bech32 DRep ID from a credential object.
   * Credential is expected to be either `{ keyHash: "hex" }` or `{ scriptHash: "hex" }`.
   * Uses Cardano.DRepID.cip129FromCredential from @cardano-sdk/core.
   */
  private _credentialToDRepId(
    credential: Record<string, unknown>,
    index: number
  ): string {
    try {
      if (credential.keyHash && typeof credential.keyHash === 'string') {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        return Cardano.DRepID.cip129FromCredential({
          type: Cardano.CredentialType.KeyHash,
          hash: credential.keyHash,
        } as any);
      }
      if (credential.scriptHash && typeof credential.scriptHash === 'string') {
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        return Cardano.DRepID.cip129FromCredential({
          type: Cardano.CredentialType.ScriptHash,
          hash: credential.scriptHash,
        } as any);
      }
    } catch (err) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `DRep entry at index ${index} has an unparseable credential`,
        err instanceof Error ? err.message : undefined
      );
    }
    throw new GovernanceQueryError(
      GovernanceQueryErrorType.ParseFailed,
      `DRep entry at index ${index} has an unknown credential shape — expected { keyHash } or { scriptHash }`
    );
  }

  private _parseRequiredEpochValue(
    rawValue: unknown,
    errorMessage: string
  ): number {
    if (typeof rawValue === 'number') {
      if (Number.isFinite(rawValue) && Number.isInteger(rawValue)) {
        return rawValue;
      }
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        errorMessage
      );
    }

    if (typeof rawValue === 'string' && /^\d+$/.test(rawValue)) {
      return Number(rawValue);
    }

    throw new GovernanceQueryError(
      GovernanceQueryErrorType.ParseFailed,
      errorMessage
    );
  }

  private _parseAnchor(
    state: Record<string, unknown>,
    index: number
  ): DRepDirectoryEntry['anchor'] {
    const anchor = state.anchor as Record<string, unknown> | null | undefined;
    if (!anchor) return null;
    const urlRaw = anchor.url ?? anchor.anchorUrl ?? anchor['anchor-url'];
    const hashRaw =
      anchor.hash ??
      anchor.anchorHash ??
      anchor['anchor-hash'] ??
      anchor.dataHash;

    if (urlRaw == null && hashRaw == null) return null;

    if (typeof urlRaw !== 'string' || typeof hashRaw !== 'string') {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `DRep entry at index ${index} has invalid anchor field types`
      );
    }

    if (!urlRaw && !hashRaw) return null;

    if (!urlRaw || !hashRaw) {
      throw new GovernanceQueryError(
        GovernanceQueryErrorType.ParseFailed,
        `DRep entry at index ${index} has partial anchor data`
      );
    }

    return { url: urlRaw, hash: hashRaw };
  }
}

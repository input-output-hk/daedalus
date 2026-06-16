import { spawn } from 'child_process';
import JSONbig from 'json-bigint';
import { Cardano } from '@cardano-sdk/core';
import { logger } from '../utils/logging';
import { NetworkMagics } from '../../common/types/cardano-node.types';
import {
  GovernanceQueryErrorType,
  DRepListQueryPayload,
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

  /** CLI subprocess timeout budget (ms) per the shared-design-tokens refresh contract. */
  private static readonly CLI_TIMEOUT_MS = 10_000;

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
  private inFlightRefresh: Promise<DRepListQueryPayload> | null = null;

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
    this.inFlightRefresh = null;
    this.nodeSocketPath = null;
    this.isSelfnode = false;
    this.networkFlag = null;
  }

  /**
   * Fetch the full DRep list from the local node.
   * Deduplicates in-flight requests — if a refresh is already running,
   * the same promise is returned to all concurrent callers.
   *
   * @throws {GovernanceQueryError} on socket-unavailable, CLI-not-found,
   *         query-failed, parse-failed, or timeout.
   */
  async fetchDRepList(): Promise<DRepListQueryPayload> {
    // Deduplicate in-flight requests
    if (this.inFlightRefresh) {
      return this.inFlightRefresh;
    }

    this.inFlightRefresh = this._doFetchDRepList();

    try {
      const result = await this.inFlightRefresh;
      this.lastSuccessfulData = result;
      return result;
    } finally {
      this.inFlightRefresh = null;
    }
  }

  /** Return the last successful query result, or null if none exists. */
  getLastSuccessfulData(): DRepListQueryPayload | null {
    return this.lastSuccessfulData;
  }

  // ---- Private Implementation ----

  private async _doFetchDRepList(): Promise<DRepListQueryPayload> {
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

    try {
      const [drepStateStdout, tipStdout] = await Promise.all([
        this._runCliQueryWithEraFallback([
          'query',
          'drep-state',
          '--all-dreps',
          '--include-stake',
          '--output-json',
        ]),
        this._runCliQueryWithEraFallback(['query', 'tip', '--output-json']),
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

  /**
   * Run a governance query with the preferred `latest` era flag.
   * Falls back to `conway` only when the installed cardano-cli rejects `latest`.
   */
  private async _runCliQueryWithEraFallback(args: string[]): Promise<string> {
    try {
      return await this._runCliQuery(['latest', ...args]);
    } catch (error) {
      if (this._shouldRetryWithConway(error)) {
        logger.warn(
          'GovernanceQueryService: retrying governance query with conway era flag',
          { args }
        );
        return this._runCliQuery(['conway', ...args]);
      }
      throw error;
    }
  }

  private _shouldRetryWithConway(error: unknown): boolean {
    if (!(error instanceof GovernanceQueryError)) {
      return false;
    }

    if (error.queryErrorType !== GovernanceQueryErrorType.QueryFailed) {
      return false;
    }

    const failureText = `${error.message}\n${
      error.details ?? ''
    }`.toLowerCase();
    return (
      failureText.includes('latest') &&
      /(invalid|unknown|expected|expecting|conway|era)/.test(failureText)
    );
  }

  /**
   * Spawn cardano-cli to query the local node.
   * Sets CARDANO_NODE_SOCKET_PATH in the child process environment — never as
   * a user-controllable argv flag.
   */
  private _runCliQuery(args: string[]): Promise<string> {
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
            `cardano-cli DRep query timed out after ${GovernanceQueryService.CLI_TIMEOUT_MS}ms`
          )
        );
      }, GovernanceQueryService.CLI_TIMEOUT_MS);

      child.on('close', (code) => {
        if (timeout) clearTimeout(timeout);
        if (code !== 0) {
          reject(
            new GovernanceQueryError(
              GovernanceQueryErrorType.QueryFailed,
              `cardano-cli exited with code ${code}`,
              stderr.trim() || undefined
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
   * Parse the raw JSON stdout from `cardano-cli latest query drep-state --all-dreps --include-stake --output-json`.
   *
   * The CLI output is an array of tuples: `[[credential, state], ...]` where:
   * - `credential` is `{ keyHash: "hex" }` or `{ scriptHash: "hex" }`
   * - `state` has `expiry` (epoch number), `anchor` (object|null), `deposit` (lovelace),
   *   and optional `stake` (lovelace string) only when `--include-stake` is used.
   *
   * DRep IDs are derived from credentials using `Cardano.DRepID.cip129FromCredential`.
   * Status is conservatively derived from `expiry` vs `currentEpoch`.
   * Voting power is nullable when `stake` is absent.
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

        // Voting power from optional stake (only with --include-stake)
        const votingPower: string | null =
          state.stake !== undefined && state.stake !== null
            ? String(state.stake)
            : null;

        const anchor = this._parseAnchor(state, index);

        return { drepId, votingPower, status, drepActivity, anchor };
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

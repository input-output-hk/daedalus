/**
 * Comprehensive unit tests for GovernanceQueryService slice-1 repair pass.
 *
 * Tests cover: successful tuple parsing, parse failure on malformed tuples /
 * unknown credential shape, timeout emission, cache reset behavior, selfnode
 * guard, and epoch derivation from tip.
 *
 * Uses deterministic jest.mock over child_process.spawn — no real subprocess spawning.
 *
 * @jest-environment node
 */
import { EventEmitter } from 'events';
import {
  GovernanceQueryService,
  GovernanceQueryError,
} from '../../../source/main/governance/GovernanceQueryService';
import { GovernanceQueryErrorType } from '../../../source/common/types/governance.types';

// We import spawn so we can mock it — jest.mock is hoisted above imports.
import * as childProcess from 'child_process';
import fs from 'fs';
import path from 'path';
import { Cardano } from '@cardano-sdk/core';

jest.mock('child_process', () => {
  const actual = jest.requireActual('child_process');
  return {
    ...actual,
    spawn: jest.fn(),
  };
});

const mockSpawn = childProcess.spawn as jest.Mock;

// ---- Mock fixtures ----

/** Realistic drep-state tuple output from cardano-cli (registration phase). */
const VALID_DREP_STATE_JSON = JSON.stringify([
  [
    { keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4' },
    {
      anchor: {
        dataHash:
          '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
        url: 'https://governance-preview.example.org/dreps/ledger-policy-lab.json',
      },
      deposit: 500000000,
      expiry: 535,
    },
  ],
  [
    { scriptHash: 'c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6' },
    {
      anchor: null,
      deposit: 500000000,
      expiry: 520,
    },
  ],
]);

/** Realistic tip output from cardano-cli query tip. */
const VALID_TIP_JSON = JSON.stringify({
  epoch: 512,
  hash: 'abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890',
  slot: 12345678,
  block: 9876543,
  era: 'Conway',
});

const LATEST_ALIAS_MISSING_STDERR =
  'Invalid argument `latest`\nExpected one of: conway';

/** Node-side failure that mentions era words but is not an argv rejection. */
const NODE_QUERY_FAILURE_STDERR =
  'MuxError MuxBearerClosed: the latest era ledger query failed unexpectedly';

/** Canonical object-map drep-stake-distribution output (committed mock). */
const STAKE_DISTRIBUTION_FIXTURE = fs.readFileSync(
  path.join(__dirname, '../../mocks/governance/drep-stake-distribution.json'),
  'utf-8'
);

/** DRep state as a non-array (invalid). */
const NON_ARRAY_DREP_STATE = JSON.stringify({ dreps: 'not-an-array' });

/** DRep state with a malformed tuple (missing second element). */
const MALFORMED_TUPLE_DREP_STATE = JSON.stringify([
  [{ keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4' }],
]);

/** DRep state with an unknown credential shape (no keyHash, no scriptHash). */
const UNKNOWN_CREDENTIAL_DREP_STATE = JSON.stringify([
  [{ someOtherField: 'xyz' }, { expiry: 500, deposit: 1000000 }],
]);

/** DRep state with missing expiry. */
const MISSING_EXPIRY_DREP_STATE = JSON.stringify([
  [
    { keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4' },
    { deposit: 500000000 },
  ],
]);

const BOOLEAN_EXPIRY_DREP_STATE = JSON.stringify([
  [
    { keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4' },
    { deposit: 500000000, expiry: true },
  ],
]);

// ---- Helpers ----

/** Create a mock ChildProcess that writes stdout/stderr and closes cleanly. */
function createMockChildProcess(stdout: string, exitCode = 0, stderr = '') {
  const child = new EventEmitter() as any;
  child.stdout = new EventEmitter();
  child.stderr = new EventEmitter();
  child.kill = jest.fn();

  // Write stdout and close on next tick so consumers attach listeners
  setTimeout(() => {
    child.stdout.emit('data', Buffer.from(stdout, 'utf-8'));
    if (stderr) {
      child.stderr.emit('data', Buffer.from(stderr, 'utf-8'));
    }
    child.emit('close', exitCode);
  }, 0);

  return child;
}

/** Create a mock that never closes (timeout test). */
function createNeverClosingChildProcess() {
  const child = new EventEmitter() as any;
  child.stdout = new EventEmitter();
  child.stderr = new EventEmitter();
  child.kill = jest.fn();
  return child;
}

/** Create a mock that emits an error event. */
function createErrorChildProcess(errorMessage: string) {
  const child = new EventEmitter() as any;
  child.stdout = new EventEmitter();
  child.stderr = new EventEmitter();
  child.kill = jest.fn();

  setTimeout(() => {
    child.emit('error', new Error(errorMessage));
  }, 0);

  return child;
}

// ---- Tests ----

describe('GovernanceQueryService — slice-1 repair pass', () => {
  let service: GovernanceQueryService;

  beforeEach(() => {
    jest.clearAllMocks();
    mockSpawn.mockReset();

    service = GovernanceQueryService.getInstance();
    service.reset();
    service.setNodeSocketPath('/tmp/test.sock');
    service.setCliBin('cardano-cli');
    service.setNetwork('mainnet');
    service.setSelfnodeMode(false);
  });

  // ---- selfnode guard ----

  describe('selfnode guard', () => {
    it('emits SelfnodeCliUnsupported when selfnode mode is active', async () => {
      service.setSelfnodeMode(true);

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
      });
    });

    it('emits SocketUnavailable when nodeSocketPath is null', async () => {
      service.setNodeSocketPath(null);

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
    });
  });

  // ---- successful tuple parsing ----

  describe('successful tuple parsing', () => {
    it('parses valid drep-state tuple output with tip epoch', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepRegistrations();

      expect(result.epoch).toBe(512);
      expect(result.dreps).toHaveLength(2);
      expect(result.fetchedAt).toBeGreaterThan(0);

      // First DRep (keyHash, with anchor); voting power is a Phase-2 concern
      const drep0 = result.dreps[0];
      expect(drep0.drepId).toMatch(/^drep1/);
      expect(drep0.votingPower).toBeNull();
      expect(drep0.status).toBe('active');
      expect(typeof drep0.drepActivity).toBe('number');
      expect(drep0.drepActivity as number).toBe(23); // 535 - 512
      expect(drep0.anchor).toEqual({
        url: 'https://governance-preview.example.org/dreps/ledger-policy-lab.json',
        hash: '6a5e200d2f3a1020202020202020202020202020202020202020202020202020',
      });

      // Second DRep (scriptHash, no stake, no anchor)
      const drep1 = result.dreps[1];
      expect(drep1.drepId).toMatch(/^drep1/);
      expect(drep1.votingPower).toBeNull();
      expect(drep1.status).toBe('active');
      expect(drep1.drepActivity).toBe(8); // 520 - 512
      expect(drep1.anchor).toBeNull();
    });

    it('returns inactive status when expiry <= currentEpoch', async () => {
      const tipAt600 = JSON.stringify({ epoch: 600 });
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(tipAt600));

      const result = await service.fetchDRepRegistrations();
      expect(result.epoch).toBe(600);
      expect(result.dreps[0].status).toBe('inactive');
      expect(result.dreps[0].drepActivity).toBe(0); // max(0, 535-600)
    });

    it('retries with conway when the installed CLI rejects the latest era alias', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
        )
        .mockReturnValueOnce(
          createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
        )
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepRegistrations();

      expect(result.epoch).toBe(512);
      expect(result.dreps).toHaveLength(2);
      expect(mockSpawn).toHaveBeenNthCalledWith(
        1,
        'cardano-cli',
        [
          'latest',
          'query',
          'drep-state',
          '--all-dreps',
          '--output-json',
          '--mainnet',
        ],
        expect.any(Object)
      );
      expect(mockSpawn).toHaveBeenNthCalledWith(
        2,
        'cardano-cli',
        ['latest', 'query', 'tip', '--output-json', '--mainnet'],
        expect.any(Object)
      );
      expect(mockSpawn).toHaveBeenNthCalledWith(
        3,
        'cardano-cli',
        [
          'conway',
          'query',
          'drep-state',
          '--all-dreps',
          '--output-json',
          '--mainnet',
        ],
        expect.any(Object)
      );
      expect(mockSpawn).toHaveBeenNthCalledWith(
        4,
        'cardano-cli',
        ['conway', 'query', 'tip', '--output-json', '--mainnet'],
        expect.any(Object)
      );
    });

    it('fails when query tip is unparseable', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess('not json'));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('fails when query tip epoch is a coercible non-numeric type', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(
          createMockChildProcess(JSON.stringify({ epoch: true }))
        );

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('caches lastSuccessfulData after a successful fetch', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepRegistrations();
      const cached = service.getLastSuccessfulData();
      expect(cached).toEqual(result);
    });

    it('reports the measured registration duration as a plain millisecond number', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepRegistrations();

      expect(typeof result.elapsedMs).toBe('number');
      expect(result.elapsedMs).toBeGreaterThanOrEqual(0);
      expect(result.elapsedMs).toBeLessThan(
        (GovernanceQueryService as any).REGISTRATION_TIMEOUT_MS
      );
    });
  });

  // ---- parse failures ----

  describe('parse failure behavior', () => {
    it('throws ParseFailed on non-array drep-state', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(NON_ARRAY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on malformed tuple (not a 2-tuple)', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(MALFORMED_TUPLE_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on unknown credential shape', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess(UNKNOWN_CREDENTIAL_DREP_STATE)
        )
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on missing expiry', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(MISSING_EXPIRY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on coercible non-numeric expiry values', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(BOOLEAN_EXPIRY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on partial anchor data', async () => {
      const PARTIAL_ANCHOR_DREP_STATE = JSON.stringify([
        [
          {
            keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4',
          },
          {
            anchor: {
              url: 'https://governance-preview.example.org/dreps/partial.json',
            },
            deposit: 500000000,
            expiry: 520,
          },
        ],
      ]);

      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(PARTIAL_ANCHOR_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on non-string anchor field types', async () => {
      const INVALID_ANCHOR_TYPE_DREP_STATE = JSON.stringify([
        [
          {
            keyHash: 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4',
          },
          {
            anchor: {
              dataHash: 12345,
              url: {
                href: 'https://governance-preview.example.org/dreps/invalid.json',
              },
            },
            deposit: 500000000,
            expiry: 520,
          },
        ],
      ]);

      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess(INVALID_ANCHOR_TYPE_DREP_STATE)
        )
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on invalid JSON', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess('{ not valid json }'))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });
  });

  // ---- timeout ----

  describe('timeout behavior', () => {
    it('emits Timeout after the 10s registration budget when the CLI never responds', async () => {
      jest.useFakeTimers();

      mockSpawn
        .mockReturnValueOnce(createNeverClosingChildProcess())
        .mockReturnValueOnce(createNeverClosingChildProcess());

      const fetchPromise = service.fetchDRepRegistrations();

      // Advance fake timers past the 10s timeout
      jest.advanceTimersByTime(10_001);
      // Flush pending microtasks
      await Promise.resolve();

      await expect(fetchPromise).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.Timeout,
      });

      jest.useRealTimers();
    });

    it('pins the per-phase budgets to the design-token contract', () => {
      expect((GovernanceQueryService as any).REGISTRATION_TIMEOUT_MS).toBe(
        10_000
      );
      expect((GovernanceQueryService as any).STAKE_TIMEOUT_MS).toBe(30_000);
    });

    it('gives the stake phase its full 30s budget before timing out', async () => {
      jest.useFakeTimers();

      mockSpawn.mockReturnValueOnce(createNeverClosingChildProcess());

      let settled = false;
      const fetchPromise = service.fetchDRepStake();
      fetchPromise.catch(() => {
        settled = true;
      });

      // Past the registration budget the stake query must still be running.
      jest.advanceTimersByTime(10_001);
      await Promise.resolve();
      await Promise.resolve();
      expect(settled).toBe(false);

      jest.advanceTimersByTime(20_000);
      await Promise.resolve();

      await expect(fetchPromise).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.Timeout,
      });

      jest.useRealTimers();
    });
  });

  // ---- CliNotFound ----

  describe('CLI error surface', () => {
    it('rejects with CliNotFound when spawn emits error', async () => {
      mockSpawn
        .mockReturnValueOnce(createErrorChildProcess('ENOENT'))
        .mockReturnValueOnce(createErrorChildProcess('ENOENT'));

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.CliNotFound,
      });
    });
  });

  // ---- cache reset behavior ----

  describe('reset() boundary', () => {
    it('clears lastSuccessfulData, socket path, and selfnode mode', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));
      await service.fetchDRepRegistrations();
      expect(service.getLastSuccessfulData()).not.toBeNull();

      service.reset();

      expect(service.getLastSuccessfulData()).toBeNull();

      // After reset, socket is null — fetchDRepRegistrations should emit SocketUnavailable
      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
    });

    it('reset clears inFlightRefresh so subsequent calls start fresh', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      // Complete first fetch then reset
      const first = await service.fetchDRepRegistrations();
      expect(first.dreps).toHaveLength(2);

      service.reset();

      // After reset, lastSuccessfulData is cleared and socket is null
      expect(service.getLastSuccessfulData()).toBeNull();
      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
    });
  });

  // ---- in-flight deduplication ----

  describe('in-flight deduplication', () => {
    it('returns the same promise for concurrent fetchDRepRegistrations calls', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const promise1 = service.fetchDRepRegistrations();
      const promise2 = service.fetchDRepRegistrations();

      // Deduplication: both should resolve to the same result
      const [r1, r2] = await Promise.all([promise1, promise2]);
      expect(r1).toBe(r2);

      // Only 2 spawn calls should have been made (one drep-state, one tip)
      expect(mockSpawn).toHaveBeenCalledTimes(2);
    });
  });

  // ---- stake distribution phase ----

  describe('stake distribution phase', () => {
    const KEY_HASH = 'a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4';
    const SCRIPT_HASH =
      'c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6';
    // Ids derived through the same production path the parser uses, so the
    // merge-key alignment is proven rather than assumed.
    const keyHashDRepId = Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: KEY_HASH,
    } as any);
    const scriptHashDRepId = Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.ScriptHash,
      hash: SCRIPT_HASH,
    } as any);

    it('parses the object-map container shape from the committed fixture', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      const result = await service.fetchDRepStake();

      expect(result.fetchedAt).toBeGreaterThan(0);
      expect(result.stakeByDRepId[keyHashDRepId]).toBe('23137980123456');
      expect(result.stakeByDRepId[scriptHashDRepId]).toBe('9007199254740993');
    });

    it('skips the two voting sentinels without creating entries', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      const result = await service.fetchDRepStake();

      expect(Object.keys(result.stakeByDRepId)).toHaveLength(2);
    });

    it('reports the measured stake duration as a plain millisecond number', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      const result = await service.fetchDRepStake();

      expect(typeof result.elapsedMs).toBe('number');
      expect(result.elapsedMs).toBeGreaterThanOrEqual(0);
    });

    it('parses the array-of-pairs container shape', async () => {
      const arrayShape = `[
        ["drep-keyHash-${KEY_HASH}", 23137980123456],
        ["drep-alwaysAbstain", 5000000000]
      ]`;
      mockSpawn.mockReturnValueOnce(createMockChildProcess(arrayShape));

      const result = await service.fetchDRepStake();

      expect(result.stakeByDRepId).toEqual({
        [keyHashDRepId]: '23137980123456',
      });
    });

    it('preserves oversized unquoted lovelace values through json-bigint parsing', async () => {
      const oversized = `{ "drep-keyHash-${KEY_HASH}": 9007199254740993 }`;
      mockSpawn.mockReturnValueOnce(createMockChildProcess(oversized));

      const result = await service.fetchDRepStake();

      expect(result.stakeByDRepId[keyHashDRepId]).toBe('9007199254740993');
    });

    it('throws ParseFailed on an unknown stake key shape', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess('{ "pool-keyHash-abc123": 42 }')
      );

      await expect(service.fetchDRepStake()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('builds the exact bulk argv with era token leading and network flag trailing', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      await service.fetchDRepStake();

      expect(mockSpawn).toHaveBeenCalledTimes(1);
      expect(mockSpawn).toHaveBeenCalledWith(
        'cardano-cli',
        [
          'latest',
          'query',
          'drep-stake-distribution',
          '--all-dreps',
          '--output-json',
          '--mainnet',
        ],
        expect.any(Object)
      );
    });

    it('retries the stake query with conway when the CLI rejects the latest alias', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
        )
        .mockReturnValueOnce(
          createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
        );

      const result = await service.fetchDRepStake();

      expect(result.stakeByDRepId[keyHashDRepId]).toBe('23137980123456');
      const secondCallArgs = mockSpawn.mock.calls[1][1] as string[];
      expect(secondCallArgs[0]).toBe('conway');
    });

    it('deduplicates concurrent stake fetches per phase', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess(STAKE_DISTRIBUTION_FIXTURE)
      );

      const [r1, r2] = await Promise.all([
        service.fetchDRepStake(),
        service.fetchDRepStake(),
      ]);

      expect(r1).toBe(r2);
      expect(mockSpawn).toHaveBeenCalledTimes(1);
    });

    it('guards selfnode and missing socket like the registration phase', async () => {
      service.setSelfnodeMode(true);
      await expect(service.fetchDRepStake()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
      });

      service.setSelfnodeMode(false);
      service.setNodeSocketPath(null);
      await expect(service.fetchDRepStake()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
      expect(mockSpawn).not.toHaveBeenCalled();
    });
  });

  // ---- era-retry signal ----

  describe('era-retry signal', () => {
    it('classifies an argv usage rejection as UsageError and still retries with conway', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
        )
        .mockReturnValueOnce(
          createMockChildProcess('', 1, LATEST_ALIAS_MISSING_STDERR)
        );

      // The conway retry also rejects, so the classified error surfaces.
      await expect(service.fetchDRepStake()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.UsageError,
      });
      expect(mockSpawn).toHaveBeenCalledTimes(2);
      const retryArgs = mockSpawn.mock.calls[1][1] as string[];
      expect(retryArgs[0]).toBe('conway');
    });

    it('does not retry with conway when both queries fail with a non-era QueryFailed', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
        )
        .mockReturnValueOnce(
          createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
        );

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.QueryFailed,
      });
      // Both parallel phase-1 queries spawned once each — no conway retry,
      // even though the stderr contains the words "latest" and "era".
      expect(mockSpawn).toHaveBeenCalledTimes(2);
    });

    it('does not retry the stake query on a non-era failure', async () => {
      mockSpawn.mockReturnValueOnce(
        createMockChildProcess('', 1, NODE_QUERY_FAILURE_STDERR)
      );

      await expect(service.fetchDRepStake()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.QueryFailed,
      });
      expect(mockSpawn).toHaveBeenCalledTimes(1);
    });
  });

  // ---- network flag injection (FP-1) ----

  describe('network flag', () => {
    it('appends the --mainnet flag after the subcommand args for the mainnet cluster', async () => {
      service.setNetwork('mainnet');
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await service.fetchDRepRegistrations();

      const firstCallArgs = mockSpawn.mock.calls[0][1] as string[];
      // The era token leads; the network flag is a per-subcommand option appended
      // at the tail (cardano-cli rejects it as a top-level/prepended option).
      expect(firstCallArgs[0]).toBe('latest');
      expect(firstCallArgs[firstCallArgs.length - 1]).toBe('--mainnet');
    });

    it('appends the --testnet-magic 1 flag tokens after the subcommand args for the preprod cluster', async () => {
      service.setNetwork('preprod');
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await service.fetchDRepRegistrations();

      const firstCallArgs = mockSpawn.mock.calls[0][1] as string[];
      expect(firstCallArgs.slice(0, 2)).toEqual(['latest', 'query']);
      expect(firstCallArgs.slice(-2)).toEqual(['--testnet-magic', '1']);
    });

    it('rejects when the network flag was never set', async () => {
      // Override the beforeEach network setup: reset clears networkFlag, then
      // re-establish socket/cli/selfnode but deliberately skip setNetwork.
      service.reset();
      service.setNodeSocketPath('/tmp/test.sock');
      service.setCliBin('cardano-cli');
      service.setSelfnodeMode(false);

      await expect(service.fetchDRepRegistrations()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.QueryFailed,
      });
      // The CLI must never be spawned without a network flag.
      expect(mockSpawn).not.toHaveBeenCalled();
    });
  });
});

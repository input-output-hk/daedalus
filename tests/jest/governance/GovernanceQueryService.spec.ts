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

jest.mock('child_process', () => {
  const actual = jest.requireActual('child_process');
  return {
    ...actual,
    spawn: jest.fn(),
  };
});

const mockSpawn = childProcess.spawn as jest.Mock;

// ---- Mock fixtures ----

/** Realistic drep-state tuple output from cardano-cli --include-stake. */
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
      stake: '23137980123456',
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

const RAW_OVERSIZED_STAKE_DREP_STATE_JSON = `[
  [
    { "keyHash": "a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4e5f6a1b2c3d4" },
    {
      "anchor": null,
      "deposit": 500000000,
      "expiry": 535,
      "stake": 9007199254740993
    }
  ]
]`;

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

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SelfnodeCliUnsupported,
      });
    });

    it('emits SocketUnavailable when nodeSocketPath is null', async () => {
      service.setNodeSocketPath(null);

      await expect(service.fetchDRepList()).rejects.toMatchObject({
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

      const result = await service.fetchDRepList();

      expect(result.epoch).toBe(512);
      expect(result.dreps).toHaveLength(2);
      expect(result.fetchedAt).toBeGreaterThan(0);

      // First DRep (keyHash, with stake and anchor)
      const drep0 = result.dreps[0];
      expect(drep0.drepId).toMatch(/^drep1/);
      expect(drep0.votingPower).toBe('23137980123456');
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

      const result = await service.fetchDRepList();
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

      const result = await service.fetchDRepList();

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
          '--include-stake',
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
          '--include-stake',
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

    it('preserves oversized unquoted lovelace values through json-bigint parsing', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess(RAW_OVERSIZED_STAKE_DREP_STATE_JSON)
        )
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepList();

      expect(result.dreps[0].votingPower).toBe('9007199254740993');
    });

    it('fails when query tip is unparseable', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess('not json'));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('fails when query tip epoch is a coercible non-numeric type', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(
          createMockChildProcess(JSON.stringify({ epoch: true }))
        );

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('caches lastSuccessfulData after a successful fetch', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const result = await service.fetchDRepList();
      const cached = service.getLastSuccessfulData();
      expect(cached).toEqual(result);
    });
  });

  // ---- parse failures ----

  describe('parse failure behavior', () => {
    it('throws ParseFailed on non-array drep-state', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(NON_ARRAY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on malformed tuple (not a 2-tuple)', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(MALFORMED_TUPLE_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on unknown credential shape', async () => {
      mockSpawn
        .mockReturnValueOnce(
          createMockChildProcess(UNKNOWN_CREDENTIAL_DREP_STATE)
        )
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on missing expiry', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(MISSING_EXPIRY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on coercible non-numeric expiry values', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(BOOLEAN_EXPIRY_DREP_STATE))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
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

      await expect(service.fetchDRepList()).rejects.toMatchObject({
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

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });

    it('throws ParseFailed on invalid JSON', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess('{ not valid json }'))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.ParseFailed,
      });
    });
  });

  // ---- timeout ----

  describe('timeout behavior', () => {
    it('emits Timeout after CLI_TIMEOUT_MS when CLI never responds', async () => {
      jest.useFakeTimers();

      mockSpawn
        .mockReturnValueOnce(createNeverClosingChildProcess())
        .mockReturnValueOnce(createNeverClosingChildProcess());

      const fetchPromise = service.fetchDRepList();

      // Advance fake timers past the 10s timeout
      jest.advanceTimersByTime(10_001);
      // Flush pending microtasks
      await Promise.resolve();

      await expect(fetchPromise).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.Timeout,
      });

      jest.useRealTimers();
    });

    it('has a static CLI_TIMEOUT_MS matching the design token budget', () => {
      expect((GovernanceQueryService as any).CLI_TIMEOUT_MS).toBe(10_000);
    });
  });

  // ---- CliNotFound ----

  describe('CLI error surface', () => {
    it('rejects with CliNotFound when spawn emits error', async () => {
      mockSpawn
        .mockReturnValueOnce(createErrorChildProcess('ENOENT'))
        .mockReturnValueOnce(createErrorChildProcess('ENOENT'));

      await expect(service.fetchDRepList()).rejects.toMatchObject({
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
      await service.fetchDRepList();
      expect(service.getLastSuccessfulData()).not.toBeNull();

      service.reset();

      expect(service.getLastSuccessfulData()).toBeNull();

      // After reset, socket is null — fetchDRepList should emit SocketUnavailable
      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
    });

    it('reset clears inFlightRefresh so subsequent calls start fresh', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      // Complete first fetch then reset
      const first = await service.fetchDRepList();
      expect(first.dreps).toHaveLength(2);

      service.reset();

      // After reset, lastSuccessfulData is cleared and socket is null
      expect(service.getLastSuccessfulData()).toBeNull();
      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.SocketUnavailable,
      });
    });
  });

  // ---- in-flight deduplication ----

  describe('in-flight deduplication', () => {
    it('returns the same promise for concurrent fetchDRepList calls', async () => {
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      const promise1 = service.fetchDRepList();
      const promise2 = service.fetchDRepList();

      // Deduplication: both should resolve to the same result
      const [r1, r2] = await Promise.all([promise1, promise2]);
      expect(r1).toBe(r2);

      // Only 2 spawn calls should have been made (one drep-state, one tip)
      expect(mockSpawn).toHaveBeenCalledTimes(2);
    });
  });

  // ---- network flag injection (FP-1) ----

  describe('network flag', () => {
    it('appends the --mainnet flag after the subcommand args for the mainnet cluster', async () => {
      service.setNetwork('mainnet');
      mockSpawn
        .mockReturnValueOnce(createMockChildProcess(VALID_DREP_STATE_JSON))
        .mockReturnValueOnce(createMockChildProcess(VALID_TIP_JSON));

      await service.fetchDRepList();

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

      await service.fetchDRepList();

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

      await expect(service.fetchDRepList()).rejects.toMatchObject({
        queryErrorType: GovernanceQueryErrorType.QueryFailed,
      });
      // The CLI must never be spawned without a network flag.
      expect(mockSpawn).not.toHaveBeenCalled();
    });
  });
});

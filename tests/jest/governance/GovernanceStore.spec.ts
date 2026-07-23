import BigNumber from 'bignumber.js';
import GovernanceStore, {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import { logger } from '../../../source/renderer/app/utils/logging';
import {
  governanceDRepListChannel,
  governanceDRepStakeChannel,
} from '../../../source/renderer/app/ipc/governanceChannel';
import {
  GovernanceQueryErrorType,
  DRepDirectoryEntry,
} from '../../../source/common/types/governance.types';

// Mock the IPC channel so fetchDRepList never reaches Electron's ipcRenderer.
jest.mock('../../../source/renderer/app/ipc/governanceChannel', () => ({
  governanceDRepListChannel: { request: jest.fn() },
  governanceDRepStakeChannel: { request: jest.fn() },
}));

// The real renderer logger writes through global.electronLog, which does not
// exist in the Jest environment; the mock records calls for the assertions.
jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

const mockRequest = governanceDRepListChannel.request as jest.Mock;
const mockStakeRequest = governanceDRepStakeChannel.request as jest.Mock;

/** Drain pending async continuations behind a macrotask boundary. */
const flushAsync = () => new Promise((resolve) => setTimeout(resolve, 0));

const DREP_ID = 'drep1xj23tk3yqyv7cqv7jn9mkz6xq8c7e5m3s2w1v0p9n8m7l6k5j';

const phase1Payload = () => ({
  dreps: [
    {
      anchor: null,
      drepActivity: 8,
      drepId: DREP_ID,
      status: 'active' as const,
      votingPower: null,
    },
  ],
  epoch: 512,
  fetchedAt: 1_750_000_000_000,
});

describe('GovernanceStore', () => {
  beforeEach(() => {
    // Reset between cases so the never-resolving impl from the dedup test
    // does not bleed into other cases.
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });
  it('rehydrates oversized lovelace strings into exact BigNumber values', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    const oversizedLovelace = '9007199254740993';
    const rawEntries: DRepDirectoryEntry[] = [
      {
        anchor: null,
        drepActivity: 8,
        drepId: 'drep1xj23tk3yqyv7cqv7jn9mkz6xq8c7e5m3s2w1v0p9n8m7l6k5j',
        status: 'active',
        votingPower: oversizedLovelace,
      },
    ];

    const [entry] = (store as any)._rehydrateDReps(rawEntries);

    expect(BigNumber.isBigNumber(entry.votingPower)).toBe(true);
    expect(entry.votingPower.toFixed()).toBe(oversizedLovelace);
    expect(entry.votingPower.isEqualTo(new BigNumber(oversizedLovelace))).toBe(
      true
    );
  });

  it('normalizes plain-object IPC error payloads without collapsing to UNKNOWN', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });
  });

  it('decodes JSON-wrapped IPC Error payloads correctly', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      name: 'Error',
      message: JSON.stringify({
        type: GovernanceQueryErrorType.SocketUnavailable,
        message: 'Cardano node socket path is not available.',
        details: 'CARDANO_NODE_SOCKET_PATH missing',
      }),
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.SocketUnavailable,
      message: 'Cardano node socket path is not available.',
      details: 'CARDANO_NODE_SOCKET_PATH missing',
    });
  });

  it('normalizes __governanceError marker objects directly', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      __governanceError: true,
      type: GovernanceQueryErrorType.QueryFailed,
      message: 'DRep state query failed.',
      details: 'Missing: --mainnet | --testnet-magic NATURAL',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.QueryFailed,
      message: 'DRep state query failed.',
      details: 'Missing: --mainnet | --testnet-magic NATURAL',
    });
  });

  it('resolves the error type from the queryErrorType fallback property', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    const normalized = (store as any)._normalizeError({
      queryErrorType: GovernanceQueryErrorType.Timeout,
      message: 'DRep state query timed out.',
    });

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.Timeout,
      message: 'DRep state query timed out.',
      details: undefined,
    });
  });

  it('normalizes a generic Error instance via the Error-instance fallback path', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    // A non-governance throw whose .message is neither JSON nor a marked
    // object, and which carries no type/queryErrorType, drops out of the
    // plain-object branches and lands on the `err instanceof Error` path.
    const normalized = (store as any)._normalizeError(new Error('boom'));

    expect(normalized).toEqual({
      type: GovernanceQueryErrorType.Unknown,
      message: 'boom',
      details: undefined,
    });
  });

  it('does not fetch the DRep list from setup()', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);

    store.setup();

    // The query must fire only on Governance-route entry / explicit refresh,
    // never at store init (which runs before the node is synced).
    expect(mockRequest).not.toHaveBeenCalled();
  });

  it('deduplicates concurrent route-entry / refresh fetches', () => {
    // Never resolves: holds the store in its in-flight (Loading) state so the
    // second call hits the dedup guard.
    mockRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);

    // Route entry (DRepDirectoryPage.componentDidMount) triggers the fetch.
    void store.refresh();
    // A second trigger (rapid re-entry or refresh) while Loading returns early.
    void store.refresh();

    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(store.isLoading).toBe(true);
  });

  it('paints the list from Phase 1 with null voting power, then merges stake by DRep id', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    let resolveStake: (value: unknown) => void = () => {};
    mockStakeRequest.mockImplementation(
      () =>
        new Promise((resolve) => {
          resolveStake = resolve;
        })
    );

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.fetchDRepList();
    await flushAsync();

    // Phase 1 painted: list visible, voting power still null, enrich running.
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.drepList).toHaveLength(1);
    expect(store.drepList[0].votingPower).toBeNull();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);

    resolveStake({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: { [DREP_ID]: '9007199254740993' },
    });
    await flushAsync();

    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loaded);
    expect(store.drepList[0].votingPower?.toFixed()).toBe('9007199254740993');
  });

  it('keeps voting power null for DReps absent from the stake map', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: {},
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    // Never a silent fallback to 0 — absence renders as unavailable.
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loaded);
    expect(store.drepList[0].votingPower).toBeNull();
  });

  it('keeps the painted list and flags ranking unavailable when the stake phase fails', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.drepList).toHaveLength(1);
    expect(store.drepList[0].votingPower).toBeNull();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Failed);
    expect(store.isRankingUnavailable).toBe(true);
    // A stake failure never becomes a directory error.
    expect(store.error).toBeNull();
  });

  it('deduplicates a refresh fired during the voting-power enrich window', async () => {
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.refresh();
    await flushAsync();
    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);

    void store.refresh();
    await flushAsync();

    expect(mockRequest).toHaveBeenCalledTimes(1);
  });

  it('logs only the normalized errorType from both phase failures', async () => {
    const errorSpy = jest.spyOn(logger, 'error').mockImplementation(() => {});
    const sensitive =
      'query failed for drep1qqsensitive000000000000000000000000000000000 drep-alwaysAbstain';

    mockRequest.mockRejectedValueOnce({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: sensitive,
      details: sensitive,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(errorSpy).toHaveBeenCalledWith(
      'GovernanceStore: fetchDRepList failed',
      expect.objectContaining({ errorType: 'QUERY_FAILED' })
    );
    // Phase 2 never fires after a Phase-1 failure.
    expect(mockStakeRequest).not.toHaveBeenCalled();

    errorSpy.mockClear();
    mockRequest.mockResolvedValue(phase1Payload());
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'PARSE_FAILED',
      message: sensitive,
    });
    await store.fetchDRepList();

    expect(errorSpy).toHaveBeenCalledWith(
      'GovernanceStore: voting power enrich failed',
      expect.objectContaining({ errorType: 'PARSE_FAILED' })
    );

    const serializedCalls = JSON.stringify(errorSpy.mock.calls);
    expect(serializedCalls).not.toContain('drep1qq');
    expect(serializedCalls).not.toContain('drep-alwaysAbstain');
    errorSpy.mockRestore();
  });
});

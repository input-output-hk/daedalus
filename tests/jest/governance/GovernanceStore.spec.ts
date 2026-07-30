import BigNumber from 'bignumber.js';
import { runInAction } from 'mobx';
import GovernanceStore, {
  GovernanceRefreshState,
  VotingPowerEnrichState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import type { AnchorEnrichEntry } from '../../../source/renderer/app/stores/GovernanceStore';
import { logger } from '../../../source/renderer/app/utils/logging';
import {
  governanceDRepListChannel,
  governanceDRepStakeChannel,
  governanceDRepAnchorChannel,
} from '../../../source/renderer/app/ipc/governanceChannel';
import {
  GovernanceQueryErrorType,
  AnchorFetchErrorType,
  DRepDirectoryEntry,
} from '../../../source/common/types/governance.types';

// Mock the IPC channel so fetchDRepList never reaches Electron's ipcRenderer.
jest.mock('../../../source/renderer/app/ipc/governanceChannel', () => ({
  governanceDRepListChannel: { request: jest.fn() },
  governanceDRepStakeChannel: { request: jest.fn() },
  governanceDRepAnchorChannel: { request: jest.fn() },
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
const mockAnchorRequest = governanceDRepAnchorChannel.request as jest.Mock;

/** Drain pending async continuations behind a macrotask boundary. */
const flushAsync = () => new Promise((resolve) => setTimeout(resolve, 0));

const DREP_ID = 'drep1xj23tk3yqyv7cqv7jn9mkz6xq8c7e5m3s2w1v0p9n8m7l6k5j';

const phase1Payload = () => ({
  dreps: [
    {
      anchor: null,
      verifiedName: null,
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
        verifiedName: null,
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

describe('GovernanceStore default cohort', () => {
  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });

  const drepIdAt = (i: number) =>
    `drep1cohort${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: null,
    verifiedName: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  // Stake descending with index: entry 0 is the largest, so ranks equal ids.
  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[],
    stakeByDRepId: Record<string, string>
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  it('exposes no cohort until voting-power enrichment has loaded', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockImplementation(() => new Promise(() => {}));

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    void store.fetchDRepList();
    await flushAsync();

    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loading);
    expect(store.isCohortActive).toBe(false);
    expect(store.defaultCohort).toBeNull();
    // Phase-1 full-list behavior is preserved while the enrich runs.
    expect(store.displayedDRepList).toBe(store.drepList);
  });

  it('keeps the full list displayed when the stake phase fails', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.isRankingUnavailable).toBe(true);
    expect(store.isCohortActive).toBe(false);
    expect(store.defaultCohort).toBeNull();
    expect(store.displayedDRepList).toHaveLength(1);
  });

  it('excludes the 35 largest by voting power and keeps the rest', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    const cohort = store.defaultCohort!;
    expect(cohort).toHaveLength(5);
    const cohortIds = new Set(cohort.map((e) => e.drepId));
    for (let i = 0; i < 35; i++) {
      expect(cohortIds.has(drepIdAt(i))).toBe(false);
    }
    for (let i = 35; i < 40; i++) {
      expect(cohortIds.has(drepIdAt(i))).toBe(true);
    }
  });

  it('ranks the top-35 boundary with lossless BigNumber comparison', async () => {
    // The two boundary stakes differ by one lovelace beyond Number precision,
    // and the LARGER stake sits on the LARGER drepId: a float-coerced compare
    // would tie them, fall to the drepId tie-break, and invert which entry
    // lands in the top 35.
    const dreps = Array.from({ length: 37 }, (_, i) => buildDrep(i));
    const stake: Record<string, string> = {};
    for (let i = 0; i < 34; i++) {
      stake[drepIdAt(i)] = `90071992547410${String(10 + i)}`;
    }
    stake[drepIdAt(34)] = '9007199254740992';
    stake[drepIdAt(35)] = '9007199254740993';
    stake[drepIdAt(36)] = '1000000';
    const store = await loadStore(dreps, stake);

    const cohortIds = new Set(store.defaultCohort!.map((e) => e.drepId));
    expect(cohortIds.has(drepIdAt(35))).toBe(false);
    expect(cohortIds.has(drepIdAt(34))).toBe(true);
    expect(cohortIds.has(drepIdAt(36))).toBe(true);
  });

  it('applies the eligibility floor after the exclusion: active and more than 6 epochs', async () => {
    // Sub-floor and inactive entries appear here ONLY to prove exclusion;
    // no fixture may place them inside a cohort.
    const dreps = [
      ...Array.from({ length: 35 }, (_, i) => buildDrep(i)),
      buildDrep(35, { drepActivity: 7 }),
      buildDrep(36, { drepActivity: 6 }),
      buildDrep(37, { drepActivity: 0, status: 'inactive' }),
      buildDrep(38, { drepActivity: null }),
    ];
    const store = await loadStore(dreps, stakeFor(39));

    expect(store.defaultCohort!.map((e) => e.drepId)).toEqual([drepIdAt(35)]);
  });

  it('caps the cohort at the 200 highest-ranked eligible entries', async () => {
    const dreps = Array.from({ length: 245 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(245));

    const cohort = store.defaultCohort!;
    expect(cohort).toHaveLength(200);
    const cohortIds = new Set(cohort.map((e) => e.drepId));
    expect(cohortIds.has(drepIdAt(35))).toBe(true);
    expect(cohortIds.has(drepIdAt(234))).toBe(true);
    expect(cohortIds.has(drepIdAt(235))).toBe(false);
    expect(cohortIds.has(drepIdAt(244))).toBe(false);
  });

  it('derives a stable order from the session seed', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const storeA = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeA.cohortSeed = 7;
    });
    const first = storeA.defaultCohort!.map((e) => e.drepId);

    expect(storeA.defaultCohort!.map((e) => e.drepId)).toEqual(first);

    const storeB = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeB.cohortSeed = 7;
    });
    expect(storeB.defaultCohort!.map((e) => e.drepId)).toEqual(first);

    runInAction(() => {
      storeB.cohortSeed = 8;
    });
    // Deterministic PRNG: if seeds 7 and 8 ever collide on this membership,
    // pick a different second seed rather than weakening the assertion.
    expect(storeB.defaultCohort!.map((e) => e.drepId)).not.toEqual(first);
  });

  it('keeps the display order stable when voting powers change but membership does not', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const storeA = await loadStore(dreps, stakeFor(45));
    runInAction(() => {
      storeA.cohortSeed = 7;
    });
    const before = storeA.defaultCohort!.map((e) => e.drepId);

    // Same membership, different in-cohort ranking: swap two stakes below
    // the top-35 boundary.
    const jiggled = stakeFor(45);
    const tmp = jiggled[drepIdAt(40)];
    jiggled[drepIdAt(40)] = jiggled[drepIdAt(44)];
    jiggled[drepIdAt(44)] = tmp;
    const storeB = await loadStore(dreps, jiggled);
    runInAction(() => {
      storeB.cohortSeed = 7;
    });

    expect(storeB.defaultCohort!.map((e) => e.drepId)).toEqual(before);
  });

  it('reshuffles without any IPC query and preserves membership', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(45));
    const before = store.defaultCohort!.map((e) => e.drepId);
    const seedBefore = store.cohortSeed;

    store.reshuffleCohort();

    // Reshuffle must never re-query: both channel call counts are unchanged.
    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(mockStakeRequest).toHaveBeenCalledTimes(1);
    expect(store.cohortSeed).not.toBe(seedBefore);
    const after = store.defaultCohort!.map((e) => e.drepId);
    expect([...after].sort()).toEqual([...before].sort());
  });

  it('preserves the session seed across an explicit refresh', async () => {
    const dreps = Array.from({ length: 45 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(45));
    const seedBefore = store.cohortSeed;
    const before = store.defaultCohort!.map((e) => e.drepId);

    await store.refresh();

    expect(store.cohortSeed).toBe(seedBefore);
    expect(store.defaultCohort!.map((e) => e.drepId)).toEqual(before);
    expect(mockRequest).toHaveBeenCalledTimes(2);
  });

  it('keeps excluded DReps in drepList and drepIndex', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    expect(store.drepList).toHaveLength(40);
    expect(store.drepIndex.get(drepIdAt(0))).toBeDefined();
    expect(store.defaultCohort!.map((e) => e.drepId)).not.toContain(
      drepIdAt(0)
    );
  });
});

describe('GovernanceStore cohort context', () => {
  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });

  const drepIdAt = (i: number) =>
    `drep1cohort${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: null,
    verifiedName: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  // Stake descending with index: entry 0 is the largest, so ranks equal ids.
  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[],
    stakeByDRepId: Record<string, string>
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  const loadStoreWithFailedStake = async (): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  it('takes the middle voting power as the median of an odd-sized cohort', async () => {
    // 38 entries: the top 35 are excluded, leaving a cohort of three.
    const dreps = Array.from({ length: 38 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(38));

    expect(store.defaultCohort).toHaveLength(3);
    expect(store.cohortMedianVotingPower!.isEqualTo('999964000000')).toBe(true);
  });

  it('averages the two middle voting powers for an even-sized cohort', async () => {
    const dreps = Array.from({ length: 39 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(39));

    expect(store.defaultCohort).toHaveLength(4);
    expect(store.cohortMedianVotingPower!.isEqualTo('999963500000')).toBe(true);
  });

  it('reports no median while the cohort is inactive', async () => {
    const store = await loadStoreWithFailedStake();

    expect(store.isCohortActive).toBe(false);
    expect(store.cohortMedianVotingPower).toBeNull();
  });

  it('excludes entries without voting power from the median sample', async () => {
    // Entries 36 and 37 get no stake, so their voting power stays null and
    // the median is the single powered cohort entry's value.
    const dreps = Array.from({ length: 38 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(36));

    expect(store.defaultCohort).toHaveLength(3);
    expect(store.cohortMedianVotingPower!.isEqualTo('999965000000')).toBe(true);
  });

  it('reports null cohort members while the cohort is inactive', async () => {
    const store = await loadStoreWithFailedStake();

    expect(store.cohortContext.memberIds).toBeNull();
    expect(store.cohortContext.medianVotingPower).toBeNull();
  });

  it('includes only verified anchor states in the cohort context', async () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    runInAction(() => {
      store.anchorStateByDRepId = new Map<string, AnchorEnrichEntry>([
        [
          drepIdAt(0),
          {
            state: 'verified',
            hash: 'a'.repeat(64),
            givenName: 'Ada',
            host: 'governance-preview.example.org',
          },
        ],
        [
          drepIdAt(1),
          {
            state: 'unavailable',
            hash: 'b'.repeat(64),
            reason: AnchorFetchErrorType.HashMismatch,
          },
        ],
        [drepIdAt(2), { state: 'loading', hash: 'c'.repeat(64) }],
      ]);
    });

    expect([...store.cohortContext.verifiedMetadataIds]).toEqual([drepIdAt(0)]);
  });
});

describe('GovernanceStore search and show-all seams', () => {
  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
  });

  const drepIdAt = (i: number) =>
    `drep1seam${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildDrep = (
    i: number,
    overrides: Partial<DRepDirectoryEntry> = {}
  ): DRepDirectoryEntry => ({
    anchor: null,
    verifiedName: null,
    drepActivity: 10,
    drepId: drepIdAt(i),
    status: 'active',
    votingPower: null,
    ...overrides,
  });

  const stakeFor = (count: number): Record<string, string> => {
    const map: Record<string, string> = {};
    for (let i = 0; i < count; i++) {
      map[drepIdAt(i)] = String(1_000_000_000_000 - i * 1_000_000);
    }
    return map;
  };

  const loadStore = async (
    dreps: DRepDirectoryEntry[],
    stakeByDRepId: Record<string, string>
  ): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue({
      dreps,
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId,
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  it('exposes the 35 largest ids once ranking has loaded', async () => {
    const dreps = Array.from({ length: 40 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(40));

    expect(store.top35DRepIds.size).toBe(35);
    for (let i = 0; i < 35; i++) {
      expect(store.top35DRepIds.has(drepIdAt(i))).toBe(true);
    }
    expect(store.top35DRepIds.has(drepIdAt(35))).toBe(false);
  });

  it('exposes no top-35 set when the ranking phase failed', async () => {
    mockRequest.mockResolvedValue({
      dreps: [buildDrep(0)],
      epoch: 512,
      fetchedAt: 1_750_000_000_000,
    });
    mockStakeRequest.mockRejectedValue({
      __governanceError: true,
      type: 'QUERY_FAILED',
      message: 'DRep stake query failed.',
    });

    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();

    expect(store.isRankingUnavailable).toBe(true);
    expect(store.top35DRepIds.size).toBe(0);
  });

  it('keeps full membership in showAllList including top-35, sub-floor and inactive entries', async () => {
    // Sub-floor and inactive entries appear here to prove show-all
    // reachability - they are never placed inside a cohort fixture.
    const dreps = [
      ...Array.from({ length: 36 }, (_, i) => buildDrep(i)),
      buildDrep(36, { drepActivity: 3 }),
      buildDrep(37, { drepActivity: 0, status: 'inactive' }),
    ];
    const store = await loadStore(dreps, stakeFor(38));

    const ids = new Set(store.showAllList.map((e) => e.drepId));
    expect(store.showAllList).toHaveLength(38);
    expect(ids.has(drepIdAt(0))).toBe(true);
    expect(ids.has(drepIdAt(36))).toBe(true);
    expect(ids.has(drepIdAt(37))).toBe(true);
  });

  it('orders showAllList from the session seed and reshuffles without any IPC query', async () => {
    const dreps = Array.from({ length: 20 }, (_, i) => buildDrep(i));
    const store = await loadStore(dreps, stakeFor(20));
    const before = store.showAllList.map((e) => e.drepId);

    expect(store.showAllList.map((e) => e.drepId)).toEqual(before);

    store.reshuffleCohort();

    expect(mockRequest).toHaveBeenCalledTimes(1);
    expect(mockStakeRequest).toHaveBeenCalledTimes(1);
    const after = store.showAllList.map((e) => e.drepId);
    expect([...after].sort()).toEqual([...before].sort());
  });
});

describe('GovernanceStore favorites', () => {
  const FAVORITE_ID =
    'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0001';
  const OTHER_ID = 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0002';

  // One backing record shared by any number of store instances simulates the
  // per-device electron-store surviving an app restart.
  const buildBackedApi = (initial: unknown = []) => {
    const backing = { record: initial };
    const localStorage = {
      getDRepFavorites: jest.fn(async () => backing.record),
      setDRepFavorites: jest.fn(async (ids: string[]) => {
        backing.record = ids;
      }),
    };
    return { api: { localStorage }, backing, localStorage };
  };

  const buildStore = (api: unknown) =>
    new GovernanceStore(api as any, {} as any, {} as any);

  afterEach(() => {
    (logger.debug as jest.Mock).mockClear();
    (logger.info as jest.Mock).mockClear();
    (logger.warn as jest.Mock).mockClear();
    (logger.error as jest.Mock).mockClear();
  });

  it('loads persisted favorites into the observable set on setup', async () => {
    const { api } = buildBackedApi([FAVORITE_ID]);
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    expect(store.favoriteDRepIds.size).toBe(1);
  });

  it('toggling adds then removes and persists the full array each time', async () => {
    const { api, localStorage } = buildBackedApi();
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    store.toggleFavorite(FAVORITE_ID);
    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    await flushAsync();
    expect(localStorage.setDRepFavorites).toHaveBeenCalledWith([FAVORITE_ID]);

    store.toggleFavorite(FAVORITE_ID);
    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(false);
    await flushAsync();
    expect(localStorage.setDRepFavorites).toHaveBeenLastCalledWith([]);
  });

  it('replaces the set instance on toggle so observers see a new reference', async () => {
    const { api } = buildBackedApi();
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    const before = store.favoriteDRepIds;
    store.toggleFavorite(FAVORITE_ID);

    expect(store.favoriteDRepIds).not.toBe(before);
  });

  it('restores favorites in a fresh store from the same backing record (app restart)', async () => {
    const { api } = buildBackedApi();
    const first = buildStore(api);
    first.setup();
    await flushAsync();
    first.toggleFavorite(FAVORITE_ID);
    first.toggleFavorite(OTHER_ID);
    await flushAsync();

    const second = buildStore(api);
    second.setup();
    await flushAsync();

    expect([...second.favoriteDRepIds].sort()).toEqual(
      [FAVORITE_ID, OTHER_ID].sort()
    );
  });

  it('degrades malformed records to the valid string subset', async () => {
    const { api } = buildBackedApi([FAVORITE_ID, 42, null, { a: 1 }]);
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect([...store.favoriteDRepIds]).toEqual([FAVORITE_ID]);
  });

  it('keeps an empty set when the read rejects, without logging', async () => {
    const api = {
      localStorage: {
        getDRepFavorites: jest.fn(async () => {
          throw new Error(`read failed for ${FAVORITE_ID}`);
        }),
        setDRepFavorites: jest.fn(),
      },
    };
    const store = buildStore(api);

    store.setup();
    await flushAsync();

    expect(store.favoriteDRepIds.size).toBe(0);
    expect(logger.error).not.toHaveBeenCalled();
    expect(logger.warn).not.toHaveBeenCalled();
  });

  it('keeps in-memory state when persistence fails and never logs the payload', async () => {
    const api = {
      localStorage: {
        getDRepFavorites: jest.fn(async () => []),
        setDRepFavorites: jest.fn(async () => {
          throw new Error(`write failed for ${FAVORITE_ID}`);
        }),
      },
    };
    const store = buildStore(api);
    store.setup();
    await flushAsync();

    store.toggleFavorite(FAVORITE_ID);
    await flushAsync();

    expect(store.favoriteDRepIds.has(FAVORITE_ID)).toBe(true);
    const allLoggerCalls = JSON.stringify([
      (logger.debug as jest.Mock).mock.calls,
      (logger.info as jest.Mock).mock.calls,
      (logger.warn as jest.Mock).mock.calls,
      (logger.error as jest.Mock).mock.calls,
    ]);
    expect(allLoggerCalls).not.toContain(FAVORITE_ID);
  });
});

describe('GovernanceStore anchor enrichment', () => {
  const ANCHOR_DREP_ID =
    'drep1anchor0000aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa';
  // Real preprod on-chain anchor pair from the epoch-295 drep-state sample.
  const ANCHOR = {
    hash: '9e8cb2b0f4c2ddbd9dea316b44680d8a989743868aeb40c1e6959982452f38e1',
    url:
      'https://raw.githubusercontent.com/cardano-foundation/cardano-academy/refs/heads/main/Cardano%20Academy.jsonld',
  };

  beforeEach(() => {
    mockRequest.mockReset();
    mockStakeRequest.mockReset();
    mockAnchorRequest.mockReset();
    (logger.debug as jest.Mock).mockClear();
    (logger.info as jest.Mock).mockClear();
    (logger.warn as jest.Mock).mockClear();
    (logger.error as jest.Mock).mockClear();
  });

  const listPayload = (anchor = ANCHOR) => ({
    dreps: [
      {
        anchor,
        verifiedName: null,
        drepActivity: 8,
        drepId: ANCHOR_DREP_ID,
        status: 'active' as const,
        votingPower: null,
      },
    ],
    epoch: 512,
    fetchedAt: 1_750_000_000_000,
  });

  const loadStore = async (anchor = ANCHOR): Promise<GovernanceStore> => {
    mockRequest.mockResolvedValue(listPayload(anchor));
    mockStakeRequest.mockResolvedValue({
      fetchedAt: 1_750_000_000_500,
      stakeByDRepId: {},
    });
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    await store.fetchDRepList();
    return store;
  };

  const verifiedResponse = (givenName: string | null = 'Cardano Academy') => ({
    status: 'verified' as const,
    content: { givenName },
    host: 'raw.githubusercontent.com',
    fetchedAt: 1_750_000_001_000,
  });

  it('projects a verified givenName into drepList, drepIndex and verifiedMetadataIds', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(verifiedResponse());

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    expect(mockAnchorRequest).toHaveBeenCalledWith(ANCHOR);
    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBe(
      'Cardano Academy'
    );
    expect(store.drepList[0].verifiedName).toBe('Cardano Academy');
    expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(true);
  });

  it('records an unavailable result without a name and outside verifiedMetadataIds', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue({
      status: 'unavailable',
      reason: AnchorFetchErrorType.HashMismatch,
    });

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    expect(store.anchorStateByDRepId.get(ANCHOR_DREP_ID)).toEqual({
      state: 'unavailable',
      hash: ANCHOR.hash,
      reason: AnchorFetchErrorType.HashMismatch,
    });
    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBeNull();
    expect(store.verifiedMetadataIds.has(ANCHOR_DREP_ID)).toBe(false);
  });

  it('deduplicates by hash and re-triggers only when the on-chain hash changes', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(verifiedResponse());

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);
    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    expect(mockAnchorRequest).toHaveBeenCalledTimes(1);

    await store.fetchAnchorContent(ANCHOR_DREP_ID, {
      ...ANCHOR,
      hash: 'b'.repeat(64),
    });

    expect(mockAnchorRequest).toHaveBeenCalledTimes(2);
  });

  it('settles a rejected IPC request as unavailable with the network reason', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockRejectedValue(new Error('ipc transport failed'));

    await expect(
      store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR)
    ).resolves.toBeUndefined();

    expect(store.anchorStateByDRepId.get(ANCHOR_DREP_ID)).toEqual({
      state: 'unavailable',
      hash: ANCHOR.hash,
      reason: AnchorFetchErrorType.Network,
    });
  });

  it('keeps the projected name through list and voting-power rebuilds', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(verifiedResponse());
    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    let resolveStake: (value: unknown) => void = () => {};
    mockStakeRequest.mockImplementation(
      () =>
        new Promise((resolve) => {
          resolveStake = resolve;
        })
    );
    void store.refresh();
    await flushAsync();

    // Phase 1 rebuilt the list; the projection must already be re-applied.
    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBe(
      'Cardano Academy'
    );

    resolveStake({ fetchedAt: 1_750_000_000_500, stakeByDRepId: {} });
    await flushAsync();

    expect(store.votingPowerState).toBe(VotingPowerEnrichState.Loaded);
    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBe(
      'Cardano Academy'
    );
    expect(store.drepList[0].verifiedName).toBe('Cardano Academy');
  });

  it('drops the projected name when a refresh changes the on-chain anchor hash', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(verifiedResponse());
    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);
    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBe(
      'Cardano Academy'
    );

    mockRequest.mockResolvedValue(
      listPayload({ ...ANCHOR, hash: 'c'.repeat(64) })
    );
    await store.refresh();

    expect(store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName).toBeNull();
  });

  it('clamps an oversized givenName to eighty characters with a trailing ellipsis', async () => {
    const store = await loadStore();
    mockAnchorRequest.mockResolvedValue(verifiedResponse('n'.repeat(200)));

    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    const name = store.drepIndex.get(ANCHOR_DREP_ID)?.verifiedName;
    expect(name).toHaveLength(80);
    expect(name?.endsWith('…')).toBe(true);
    expect(name?.startsWith('n'.repeat(79))).toBe(true);
  });

  it('logs nothing on the anchor path for verified or unavailable results', async () => {
    const store = await loadStore();
    (logger.error as jest.Mock).mockClear();

    mockAnchorRequest.mockResolvedValueOnce(verifiedResponse());
    await store.fetchAnchorContent(ANCHOR_DREP_ID, ANCHOR);

    mockAnchorRequest.mockResolvedValueOnce({
      status: 'unavailable',
      reason: AnchorFetchErrorType.HashMismatch,
    });
    await store.fetchAnchorContent(ANCHOR_DREP_ID, {
      ...ANCHOR,
      hash: 'd'.repeat(64),
    });

    expect(logger.debug).not.toHaveBeenCalled();
    expect(logger.info).not.toHaveBeenCalled();
    expect(logger.warn).not.toHaveBeenCalled();
    expect(logger.error).not.toHaveBeenCalled();
  });
});

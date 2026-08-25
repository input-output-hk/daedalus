import BigNumber from 'bignumber.js';
import { observable, onReactionError, runInAction } from 'mobx';
import GovernanceStore, {
  GovernanceRefreshState,
} from '../../../source/renderer/app/stores/GovernanceStore';
import type { ApiDRepInfo } from '../../../source/renderer/app/api/governance/types';
import { logger } from '../../../source/renderer/app/utils/logging';

jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    error: jest.fn(),
    warn: jest.fn(),
  },
}));

/** Drain pending async continuations behind a macrotask boundary. */
const flushAsync = () => new Promise((resolve) => setTimeout(resolve, 0));

const CURRENT_EPOCH = 512;

const drepIdAt = (i: number) =>
  `drep1cohort${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

const buildApiDRep = (
  i: number,
  overrides: Partial<ApiDRepInfo> = {}
): ApiDRepInfo => ({
  id: drepIdAt(i),
  credential: { type: 'key_hash', hash: '0'.repeat(56) },
  status: 'active',
  expiry_epoch: 522,
  voting_power: {
    quantity: String(1_000_000_000_000 - i * 1_000_000),
    unit: 'lovelace',
  },
  deposit: { quantity: 500_000_000, unit: 'lovelace' },
  anchor: null,
  name: 'Test DRep',
  do_not_list: false,
  metadata: null,
  ...overrides,
});

const makeApi = (listDRepsImpl = jest.fn().mockResolvedValue([])) => ({
  ada: {
    listDReps: listDRepsImpl,
    getDRep: jest.fn().mockResolvedValue(null),
    getDRepSummary: jest.fn().mockResolvedValue({
      total_drep_stake: { quantity: '511304929746789', unit: 'lovelace' },
      active_drep_count: 59,
      inactive_drep_count: 221,
      total_drep_count: 280,
    }),
  },
  localStorage: {
    getDRepFavorites: jest.fn().mockResolvedValue([]),
    setDRepFavorites: jest.fn().mockResolvedValue(undefined),
  },
});

const makeStores = (epoch: number | null = CURRENT_EPOCH) => ({
  networkStatus: {
    localTip: epoch != null ? { epoch } : null,
    isNodeInSync: true,
  },
  router: { location: { pathname: '/governance' } },
});

const makeStore = (api = makeApi(), epoch: number | null = CURRENT_EPOCH) => {
  const store = new GovernanceStore(api as any, {} as any, {} as any);
  store.configure(makeStores(epoch) as any);
  return store;
};

/** Load the full DRep list into the store. */
const loadStore = async (
  dreps: ApiDRepInfo[] = Array.from({ length: 40 }, (_, i) => buildApiDRep(i))
): Promise<GovernanceStore> => {
  const api = makeApi(jest.fn().mockResolvedValue(dreps));
  const store = makeStore(api);
  await store.fetchAllDReps();
  return store;
};

/**
 * Load a list the cohort can be drawn from. `buildApiDRep` satisfies every
 * default criterion, so the cohort is the whole list up to its size.
 */
const loadSuggestedStore = async (
  dreps: ApiDRepInfo[] = Array.from({ length: 20 }, (_, i) => buildApiDRep(i))
): Promise<GovernanceStore> => loadStore(dreps);

// ---------------------------------------------------------------------------
// GovernanceStore (top-level)
// ---------------------------------------------------------------------------

describe('GovernanceStore', () => {
  it('maps voting_power.quantity to an exact BigNumber', async () => {
    const oversizedLovelace = '9007199254740993';
    const drep = buildApiDRep(0, {
      voting_power: { quantity: oversizedLovelace, unit: 'lovelace' },
    });
    const store = await loadStore([drep]);
    const entry = store.allDReps.find((e) => e.drepId === drep.id);
    expect(BigNumber.isBigNumber(entry?.votingPower)).toBe(true);
    expect(entry?.votingPower?.toFixed()).toBe(oversizedLovelace);
  });

  it('normalizes plain-object API errors preserving type and details', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    const normalized = (store as any)._normalizeError({
      type: 'NETWORK_ERROR',
      message: 'Connection refused',
      details: 'ECONNREFUSED',
    });
    expect(normalized).toEqual({
      type: 'NETWORK_ERROR',
      message: 'Connection refused',
      details: 'ECONNREFUSED',
    });
  });

  it('normalizes a generic Error instance to UNKNOWN', () => {
    const store = new GovernanceStore({} as any, {} as any, {} as any);
    const normalized = (store as any)._normalizeError(new Error('boom'));
    expect(normalized).toEqual({ type: 'UNKNOWN', message: 'boom' });
  });

  it('does not fetch the DRep list from setup()', () => {
    const api = makeApi();
    const store = makeStore(api);
    store.setup();
    expect(api.ada.listDReps).not.toHaveBeenCalled();
  });

  it('deduplicates concurrent fetchAllDReps calls', () => {
    const api = makeApi(
      jest.fn().mockImplementation(() => new Promise(() => {}))
    );
    const store = makeStore(api);

    void store.fetchAllDReps();
    void store.fetchAllDReps();

    expect(api.ada.listDReps).toHaveBeenCalledTimes(1);
    expect(store.isLoading).toBe(true);
  });

  it('transitions through Refreshing on a second fetchAllDReps when data already exists', async () => {
    const api = makeApi(jest.fn().mockResolvedValueOnce([buildApiDRep(0)]));
    const store = makeStore(api);
    await store.fetchAllDReps();
    expect(store.suggestedDReps).toHaveLength(1);

    (api.ada.listDReps as jest.Mock).mockImplementation(
      () => new Promise(() => {})
    );
    void store.fetchAllDReps();

    expect(store.refreshState).toBe(GovernanceRefreshState.Refreshing);
    expect(store.suggestedDReps).toHaveLength(1);
  });

  it('sets Failed state when fetchAllDReps throws on first load', async () => {
    const api = makeApi(
      jest.fn().mockRejectedValue(new Error('Network error'))
    );
    const store = makeStore(api);
    await store.fetchAllDReps();

    expect(store.refreshState).toBe(GovernanceRefreshState.Failed);
    expect(store.hasError).toBe(true);
    expect(store.suggestedDReps.length).toBe(0);
  });

  it('retains Loaded state with old data when a refresh fails', async () => {
    const api = makeApi(jest.fn().mockResolvedValueOnce([buildApiDRep(0)]));
    const store = makeStore(api);
    await store.fetchAllDReps();
    const initialLength = store.suggestedDReps.length;

    (api.ada.listDReps as jest.Mock).mockRejectedValue(
      new Error('refresh failed')
    );
    await store.refresh();

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.suggestedDReps).toHaveLength(initialLength);
  });

  it('refresh() reloads the full list the cohort is drawn from', async () => {
    const api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]));
    const store = makeStore(api);
    await store.refresh();

    expect(api.ada.listDReps).toHaveBeenCalledTimes(1);
    expect(store.allDReps).toHaveLength(1);
    expect(store.suggestedDReps).toHaveLength(1);
  });

  it('fetchAllDReps drives the directory refresh state', async () => {
    const store = await loadStore([buildApiDRep(0)]);

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.lastFetchedAt).not.toBeNull();
  });

  it('loadAllDReps fetches only once when allDReps is already populated', async () => {
    const api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]));
    const store = makeStore(api);
    await store.loadAllDReps();
    await store.loadAllDReps();

    expect(api.ada.listDReps).toHaveBeenCalledTimes(1);
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore full list (allDReps)
// ---------------------------------------------------------------------------

describe('GovernanceStore full list', () => {
  const seamDrepIdAt = (i: number) =>
    `drep1seam${String(i).padStart(4, '0')}aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa`;

  const buildSeamApiDRep = (
    i: number,
    overrides: Partial<ApiDRepInfo> = {}
  ): ApiDRepInfo => ({
    id: seamDrepIdAt(i),
    credential: { type: 'key_hash', hash: '0'.repeat(56) },
    status: 'active',
    expiry_epoch: 522,
    voting_power: {
      quantity: String(1_000_000_000_000 - i * 1_000_000),
      unit: 'lovelace',
    },
    deposit: { quantity: 500_000_000, unit: 'lovelace' },
    anchor: null,
    name: 'Test DRep',
    do_not_list: false,
    metadata: null,
    ...overrides,
  });

  it('keeps all DReps in allDReps (including doNotList entries)', async () => {
    const store = await loadStore(
      Array.from({ length: 40 }, (_, i) => buildApiDRep(i))
    );
    expect(store.allDReps).toHaveLength(40);
    for (let i = 0; i < 40; i++) {
      expect(
        store.allDReps.find((e) => e.drepId === drepIdAt(i))
      ).toBeDefined();
    }
  });

  it('allDReps includes all DReps regardless of eligibility flags', async () => {
    const dreps = [
      buildSeamApiDRep(0, { name: null }),
      buildSeamApiDRep(1, { status: 'inactive' }),
      buildSeamApiDRep(2, { do_not_list: true }),
      buildSeamApiDRep(3),
    ];
    const api = makeApi(jest.fn().mockResolvedValue(dreps));
    const store = makeStore(api);
    await store.fetchAllDReps();

    expect(store.allDReps.length).toBe(4);
    const ids = new Set(store.allDReps.map((e) => e.drepId));
    for (let i = 0; i < 4; i++) {
      expect(ids.has(seamDrepIdAt(i))).toBe(true);
    }
  });

  it('allDReps order matches the API response order', async () => {
    const dreps = Array.from({ length: 10 }, (_, i) => buildSeamApiDRep(i));
    const api = makeApi(jest.fn().mockResolvedValue(dreps));
    const store = makeStore(api);
    await store.fetchAllDReps();

    expect(store.allDReps.map((e) => e.drepId)).toEqual(dreps.map((d) => d.id));
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore suggested DReps
// ---------------------------------------------------------------------------

describe('GovernanceStore suggested DReps', () => {
  it('draws suggestedDReps from the loaded list', async () => {
    const dreps = [buildApiDRep(0), buildApiDRep(1)];
    const store = await loadSuggestedStore(dreps);
    expect(store.suggestedDReps.map((e) => e.drepId).sort()).toEqual(
      [drepIdAt(0), drepIdAt(1)].sort()
    );
  });

  it('normalizes verifiedName from the name field', async () => {
    const drep = buildApiDRep(0, { name: 'My DRep' });
    const store = await loadSuggestedStore([drep]);
    expect(store.suggestedDReps[0].verifiedName).toBe('My DRep');
  });

  it('sets verifiedName to null when name is absent', async () => {
    const drep = buildApiDRep(0, { name: null });
    const store = await loadSuggestedStore([drep]);
    expect(store.suggestedDReps[0].verifiedName).toBeNull();
  });

  it('clamps verifiedName to 80 characters with an ellipsis', async () => {
    const longName = 'A'.repeat(90);
    const drep = buildApiDRep(0, { name: longName });
    const store = await loadSuggestedStore([drep]);
    const name = store.suggestedDReps[0].verifiedName!;
    expect(name.length).toBe(80);
    expect(name.endsWith('…')).toBe(true);
  });

  it('sets refreshState to Loaded after a successful fetch', async () => {
    const store = await loadSuggestedStore([buildApiDRep(0)]);
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.isLoaded).toBe(true);
    expect(store.isEmpty).toBe(false);
  });

  it('isEmpty is true when suggestedDReps is empty after loading', async () => {
    const store = await loadSuggestedStore([]);
    expect(store.isEmpty).toBe(true);
  });

  it('holds the cohort to its configured size', async () => {
    const store = await loadSuggestedStore(
      Array.from({ length: 40 }, (_, i) => buildApiDRep(i))
    );
    expect(store.suggestedDReps).toHaveLength(20);

    store.setCohortCriteria({ ...store.cohortCriteria, size: 10 });
    expect(store.suggestedDReps).toHaveLength(10);
  });

  it('rerolls to a different cohort without another request', async () => {
    const api = makeApi(
      jest
        .fn()
        .mockResolvedValue(
          Array.from({ length: 60 }, (_, i) => buildApiDRep(i))
        )
    );
    const store = makeStore(api);
    await store.fetchAllDReps();
    const before = store.suggestedDReps.map((e) => e.drepId);

    store.rerollCohort();

    expect(store.suggestedDReps.map((e) => e.drepId)).not.toEqual(before);
    expect(api.ada.listDReps).toHaveBeenCalledTimes(1);
  });

  it('excludes DReps that asked not to be listed', async () => {
    const store = await loadSuggestedStore([
      buildApiDRep(0),
      buildApiDRep(1, { do_not_list: true }),
    ]);
    expect(store.suggestedDReps.map((e) => e.drepId)).toEqual([drepIdAt(0)]);
    expect(store.allDReps).toHaveLength(2);
  });

  it('applies the criteria the user set rather than the defaults alone', async () => {
    const store = await loadSuggestedStore([
      buildApiDRep(0),
      buildApiDRep(1, { status: 'inactive' }),
    ]);
    // The inactive DRep is not suggested while the pool can be filled without
    // it, and the pool of one cannot fill a cohort of twenty, so the criterion
    // is relaxed and it appears. Asking for a single suggestion is enough for
    // the criterion to hold.
    store.setCohortCriteria({ ...store.cohortCriteria, size: 1 });
    expect(store.suggestedDReps.map((e) => e.drepId)).toEqual([drepIdAt(0)]);

    // The inactive DRep stays out: active is a pre-filter now, not a
    // criterion, so there is nothing to turn off that would admit it.
    store.setCohortCriteria({
      ...store.cohortCriteria,
      requireVerifiedMetadata: false,
      includeInactiveSoon: true,
    });
    expect(store.cohortPool.entries).toHaveLength(1);
  });

  it('keeps the criteria when the directory is left and reopened', async () => {
    const router = observable<{ location: { pathname: string } | null }>({
      location: { pathname: '/governance' },
    });
    const store = new GovernanceStore(makeApi() as any, {} as any, {} as any);
    store.configure({
      networkStatus: { localTip: { epoch: CURRENT_EPOCH }, isNodeInSync: true },
      router,
    } as any);
    store.setup();
    store.setCohortCriteria({ ...store.cohortCriteria, size: 10 });

    runInAction(() => {
      router.location = { pathname: '/wallets' };
    });

    expect(store.cohortCriteria.size).toBe(10);
    expect(store.allDReps).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore favorites
// ---------------------------------------------------------------------------

describe('GovernanceStore favorites', () => {
  const FAVORITE_ID =
    'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0001';
  const OTHER_ID = 'drep1yg7s8vuv_8ff8a9y6z0m8p4kw7q9s8n3d7m9p2l0v8k6m6m2k0002';

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

// ---------------------------------------------------------------------------
// GovernanceStore fetchDRep
// ---------------------------------------------------------------------------

describe('GovernanceStore fetchDRep', () => {
  const rawDRepAt = (
    i: number,
    overrides: Partial<ApiDRepInfo> = {}
  ): ApiDRepInfo => buildApiDRep(i, overrides);

  it('fetches and normalizes a DRep detail', async () => {
    const rawDRep = rawDRepAt(1, {
      name: 'Alice',
      metadata: {
        name: 'Alice',
        objectives: 'obj',
        motivations: null,
        qualifications: null,
        do_not_list: false,
        references: [],
      } as any,
    });
    const api = makeApi();
    (api.ada.getDRep as jest.Mock).mockResolvedValue(rawDRep);
    const store = makeStore(api);
    const detail = await store.fetchDRep(drepIdAt(1));
    expect(detail.drepId).toBe(drepIdAt(1));
    expect(detail.metadata?.objectives).toBe('obj');
  });

  it('throws when the API call fails', async () => {
    const api = makeApi();
    (api.ada.getDRep as jest.Mock).mockRejectedValue(new Error('not found'));
    const store = makeStore(api);
    await expect(store.fetchDRep(drepIdAt(1))).rejects.toThrow('not found');
  });

  it('does not update store state', async () => {
    const rawDRep = rawDRepAt(1, { metadata: null } as any);
    const api = makeApi();
    (api.ada.getDRep as jest.Mock).mockResolvedValue(rawDRep);
    const store = makeStore(api);
    await store.fetchDRep(drepIdAt(1));
    expect(store.allDReps).toHaveLength(0);
    expect(store.suggestedDReps).toHaveLength(0);
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore route reaction
// ---------------------------------------------------------------------------

describe('GovernanceStore route reaction', () => {
  const makeRoutedStore = (
    router: { location: { pathname: string } | null },
    api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]))
  ) => {
    const store = new GovernanceStore(api as any, {} as any, {} as any);
    store.configure({
      networkStatus: { localTip: { epoch: CURRENT_EPOCH }, isNodeInSync: true },
      router,
    } as any);
    return store;
  };

  it('reads a null router location as off the governance page', () => {
    const store = makeRoutedStore({ location: null });
    expect(store.isGovernancePage).toBe(false);
  });

  it('raises no MobX reaction error from setup() before the router holds a location', () => {
    // The reported failure surfaced as `[mobx] Encountered an uncaught
    // exception ... in: 'Reaction'`, which MobX routes to its own handler
    // rather than rethrowing, so asserting on a throw would pass either way.
    const reactionErrors: unknown[] = [];
    const stopCapturing = onReactionError((error) => {
      reactionErrors.push(error);
    });
    const store = makeRoutedStore({ location: null });

    try {
      store.setup();
    } finally {
      stopCapturing();
    }

    expect(reactionErrors).toHaveLength(0);
  });

  it('clears governance state on leaving the page, not on entering it', async () => {
    const router = observable<{ location: { pathname: string } | null }>({
      location: null,
    });
    const store = makeRoutedStore(router);
    store.setup();

    runInAction(() => {
      router.location = { pathname: '/governance' };
    });
    expect(store.isGovernancePage).toBe(true);

    await store.fetchAllDReps();
    expect(store.suggestedDReps).toHaveLength(1);
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);

    runInAction(() => {
      router.location = { pathname: '/wallets' };
    });
    expect(store.suggestedDReps).toHaveLength(0);
    expect(store.refreshState).toBe(GovernanceRefreshState.Idle);
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore DRep summary
// ---------------------------------------------------------------------------

describe('GovernanceStore DRep summary', () => {
  it('records the totals and reports the summary as available', async () => {
    const store = makeStore();
    await store.fetchDRepSummary();
    expect(store.drepSummary?.totalDRepStake.toFixed()).toBe('511304929746789');
    expect(store.drepSummary?.totalDRepCount).toBe(280);
    expect(store.drepSummaryState).toBe(GovernanceRefreshState.Loaded);
    expect(store.isDRepSummaryAvailable).toBe(true);
  });

  it('records a failure in its own state rather than swallowing it', async () => {
    // A wallet build predating /dreps/summary rejects the request on every
    // refresh; the directory must survive it, but the failure has to be
    // visible so dependent UI can explain itself.
    const api = makeApi();
    (api.ada.getDRepSummary as jest.Mock).mockRejectedValue(
      new Error('bad_request')
    );
    const store = makeStore(api);
    await store.fetchDRepSummary();
    expect(store.drepSummaryState).toBe(GovernanceRefreshState.Failed);
    expect(store.isDRepSummaryAvailable).toBe(false);
    expect(store.drepSummary).toBeNull();
  });

  it('leaves the directory loaded when the summary fails', async () => {
    const api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]));
    (api.ada.getDRepSummary as jest.Mock).mockRejectedValue(
      new Error('bad_request')
    );
    const store = makeStore(api);
    await store.fetchAllDReps();
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.suggestedDReps).toHaveLength(1);
    expect(store.error).toBeNull();
    expect(store.drepSummaryState).toBe(GovernanceRefreshState.Failed);
  });

  it('clears summary state when leaving the governance page', async () => {
    const router = observable<{ location: { pathname: string } | null }>({
      location: { pathname: '/governance' },
    });
    const store = new GovernanceStore(makeApi() as any, {} as any, {} as any);
    store.configure({
      networkStatus: { localTip: { epoch: CURRENT_EPOCH }, isNodeInSync: true },
      router,
    } as any);
    store.setup();
    await store.fetchDRepSummary();
    expect(store.isDRepSummaryAvailable).toBe(true);

    runInAction(() => {
      router.location = { pathname: '/wallets' };
    });
    expect(store.drepSummary).toBeNull();
    expect(store.drepSummaryState).toBe(GovernanceRefreshState.Idle);
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore metadata emptiness
// ---------------------------------------------------------------------------

describe('GovernanceStore metadata emptiness', () => {
  const rawWithMetadata = (metadata: Record<string, unknown>) => ({
    ...buildApiDRep(1),
    metadata,
  });

  const fetchDetail = async (metadata: Record<string, unknown>) => {
    const api = makeApi();
    (api.ada.getDRep as jest.Mock).mockResolvedValue(rawWithMetadata(metadata));
    const store = makeStore(api);
    return store.fetchDRep(drepIdAt(1));
  };

  it('treats a blank CIP-119 field as absent, so no empty row can render', async () => {
    // Review item 4: sections a DRep did not provide were still shown. A DRep
    // that published an empty string has not provided the field.
    const detail = await fetchDetail({
      objectives: '',
      motivations: '   ',
      qualifications: null,
      payment_address: '',
    });

    expect(detail.metadata?.objectives).toBeNull();
    expect(detail.metadata?.motivations).toBeNull();
    expect(detail.metadata?.qualifications).toBeNull();
    expect(detail.metadata?.paymentAddress).toBeNull();
  });

  it('keeps real values and trims them', async () => {
    const detail = await fetchDetail({
      objectives: '  Improve treasury oversight  ',
      motivations: 'Long-term stability',
    });

    expect(detail.metadata?.objectives).toBe('Improve treasury oversight');
    expect(detail.metadata?.motivations).toBe('Long-term stability');
  });

  it('drops references that have nothing to link to', async () => {
    const detail = await fetchDetail({
      references: [
        { '@type': 'Link', label: 'Site', uri: 'https://example.org' },
        { '@type': 'Link', label: 'Empty', uri: '' },
        { '@type': 'Identity', label: null, uri: '   ' },
      ],
    });

    expect(detail.metadata?.references).toHaveLength(1);
    expect(detail.metadata?.references[0].uri).toBe('https://example.org');
  });

  it('blanks a reference label without dropping the reference', async () => {
    const detail = await fetchDetail({
      references: [
        { '@type': 'Link', label: '  ', uri: 'https://example.org' },
      ],
    });

    expect(detail.metadata?.references).toHaveLength(1);
    expect(detail.metadata?.references[0].label).toBeNull();
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore pinned favourites
// ---------------------------------------------------------------------------

describe('GovernanceStore favorite entries', () => {
  it('resolves favourites the cohort does not contain', async () => {
    // The default view samples twenty at random, so a favourite is usually
    // absent from it. Resolution goes through the per-DRep cache instead.
    const api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]));
    (api.ada.getDRep as jest.Mock).mockResolvedValue(buildApiDRep(5));
    const store = makeStore(api);
    await store.fetchAllDReps();
    store.toggleFavorite(drepIdAt(5));

    expect(store.favoriteEntries).toHaveLength(0);
    await store.ensureFavorites();
    expect(store.favoriteEntries.map((e) => e.drepId)).toEqual([drepIdAt(5)]);
  });

  it('resolves a favourite already in the cohort without fetching it', async () => {
    const api = makeApi(jest.fn().mockResolvedValue([buildApiDRep(0)]));
    const store = makeStore(api);
    await store.fetchAllDReps();
    store.toggleFavorite(drepIdAt(0));

    expect(store.favoriteEntries.map((e) => e.drepId)).toEqual([drepIdAt(0)]);
    await store.ensureFavorites();
    expect(api.ada.getDRep).not.toHaveBeenCalled();
  });

  it('skips a favourite that cannot be resolved rather than rendering it half-built', async () => {
    const api = makeApi();
    (api.ada.getDRep as jest.Mock).mockRejectedValue(new Error('not found'));
    const store = makeStore(api);
    store.toggleFavorite(drepIdAt(9));

    await store.ensureFavorites();
    expect(store.favoriteEntries).toEqual([]);
  });

  it('is empty when nothing is favourited', () => {
    expect(makeStore().favoriteEntries).toEqual([]);
  });
});

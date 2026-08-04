import BigNumber from 'bignumber.js';
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
  voting_power: { quantity: String(1_000_000_000_000 - i * 1_000_000), unit: 'lovelace' },
  deposit: { quantity: 500_000_000, unit: 'lovelace' },
  anchor: null,
  name: 'Test DRep',
  do_not_list: false,
  metadata: null,
  ...overrides,
});

const makeApi = (
  listDRepsImpl = jest.fn().mockResolvedValue([]),
  listSuggestedDRepsImpl = jest.fn().mockResolvedValue([])
) => ({
  ada: {
    listDReps: listDRepsImpl,
    listSuggestedDReps: listSuggestedDRepsImpl,
    getDRep: jest.fn().mockResolvedValue(null),
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

const makeStore = (
  api = makeApi(),
  epoch: number | null = CURRENT_EPOCH
) => {
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

/** Load suggested DReps into the store. */
const loadSuggestedStore = async (
  dreps: ApiDRepInfo[] = Array.from({ length: 20 }, (_, i) => buildApiDRep(i))
): Promise<GovernanceStore> => {
  const api = makeApi(jest.fn().mockResolvedValue([]), jest.fn().mockResolvedValue(dreps));
  const store = makeStore(api);
  await store.fetchSuggestedDReps();
  return store;
};

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
    expect(api.ada.listSuggestedDReps).not.toHaveBeenCalled();
  });

  it('deduplicates concurrent fetchSuggestedDReps calls', () => {
    const api = makeApi(
      jest.fn().mockResolvedValue([]),
      jest.fn().mockImplementation(() => new Promise(() => {}))
    );
    const store = makeStore(api);

    void store.fetchSuggestedDReps();
    void store.fetchSuggestedDReps();

    expect(api.ada.listSuggestedDReps).toHaveBeenCalledTimes(1);
    expect(store.isLoading).toBe(true);
  });

  it('transitions through Refreshing on a second fetchSuggestedDReps when data already exists', async () => {
    const firstDreps = [buildApiDRep(0)];
    const api = makeApi(
      jest.fn().mockResolvedValue([]),
      jest.fn().mockResolvedValueOnce(firstDreps)
    );
    const store = makeStore(api);
    await store.fetchSuggestedDReps();
    expect(store.suggestedDReps).toHaveLength(1);

    (api.ada.listSuggestedDReps as jest.Mock).mockImplementation(
      () => new Promise(() => {})
    );
    void store.fetchSuggestedDReps();

    expect(store.refreshState).toBe(GovernanceRefreshState.Refreshing);
    expect(store.suggestedDReps).toHaveLength(1);
  });

  it('sets Failed state when fetchSuggestedDReps throws on first load', async () => {
    const api = makeApi(
      jest.fn().mockResolvedValue([]),
      jest.fn().mockRejectedValue(new Error('Network error'))
    );
    const store = makeStore(api);
    await store.fetchSuggestedDReps();

    expect(store.refreshState).toBe(GovernanceRefreshState.Failed);
    expect(store.hasError).toBe(true);
    expect(store.suggestedDReps.length).toBe(0);
  });

  it('retains Loaded state with old data when a refresh fails', async () => {
    const api = makeApi(
      jest.fn().mockResolvedValue([]),
      jest.fn().mockResolvedValueOnce([buildApiDRep(0)])
    );
    const store = makeStore(api);
    await store.fetchSuggestedDReps();
    const initialLength = store.suggestedDReps.length;

    (api.ada.listSuggestedDReps as jest.Mock).mockRejectedValue(
      new Error('refresh failed')
    );
    await store.refresh();

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.suggestedDReps).toHaveLength(initialLength);
  });

  it('refresh() calls fetchSuggestedDReps, not listDReps', async () => {
    const api = makeApi(
      jest.fn().mockResolvedValue([buildApiDRep(0)]),
      jest.fn().mockResolvedValue([buildApiDRep(1)])
    );
    const store = makeStore(api);
    await store.refresh();

    expect(api.ada.listSuggestedDReps).toHaveBeenCalledTimes(1);
    expect(api.ada.listDReps).not.toHaveBeenCalled();
    expect(store.suggestedDReps).toHaveLength(1);
  });

  it('fetchSuggestedDReps updates refreshState, not allDRepsRefreshState', async () => {
    const store = await loadSuggestedStore([buildApiDRep(0)]);

    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.allDRepsRefreshState).toBe(GovernanceRefreshState.Idle);
  });

  it('fetchAllDReps updates allDRepsRefreshState, not refreshState', async () => {
    const store = await loadStore([buildApiDRep(0)]);

    expect(store.allDRepsRefreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.refreshState).toBe(GovernanceRefreshState.Idle);
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
    voting_power: { quantity: String(1_000_000_000_000 - i * 1_000_000), unit: 'lovelace' },
    deposit: { quantity: 500_000_000, unit: 'lovelace' },
    anchor: null,
    name: 'Test DRep',
    do_not_list: false,
    ...overrides,
  });

  it('keeps all DReps in allDReps (including doNotList entries)', async () => {
    const store = await loadStore(Array.from({ length: 40 }, (_, i) => buildApiDRep(i)));
    expect(store.allDReps).toHaveLength(40);
    for (let i = 0; i < 40; i++) {
      expect(store.allDReps.find((e) => e.drepId === drepIdAt(i))).toBeDefined();
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

    expect(store.allDReps.map((e) => e.drepId)).toEqual(
      dreps.map((d) => d.id)
    );
  });
});

// ---------------------------------------------------------------------------
// GovernanceStore suggested DReps
// ---------------------------------------------------------------------------

describe('GovernanceStore suggested DReps', () => {
  it('populates suggestedDReps from fetchSuggestedDReps', async () => {
    const dreps = [buildApiDRep(0), buildApiDRep(1)];
    const store = await loadSuggestedStore(dreps);
    expect(store.suggestedDReps).toHaveLength(2);
    expect(store.suggestedDReps[0].drepId).toBe(drepIdAt(0));
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

  it('does not update allDReps when fetching suggested DReps', async () => {
    const store = await loadSuggestedStore([buildApiDRep(0)]);
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
  const rawDRepAt = (i: number, overrides: Partial<ApiDRepInfo> = {}): ApiDRepInfo =>
    buildApiDRep(i, overrides);

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

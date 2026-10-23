import { setImmediate as realSetImmediate } from 'timers';
import type AdaApiClass from '../../../source/renderer/app/api/api';
import type GovernanceStoreClass from '../../../source/renderer/app/stores/GovernanceStore';
import { knownDefect } from '../support/knownDefect';
import {
  loadRecordedExchange,
  provideRequestGlobals,
  startWalletReplay,
} from '../support/walletReplay';
import type { ReplayRoute, WalletReplay } from '../support/walletReplay';

jest.mock('../../../source/renderer/app/utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));

provideRequestGlobals();
const api = require('../../../source/renderer/app/api/api');

const AdaApi = api.default as typeof AdaApiClass;
const { _createWalletFromServerData } = api;
const governanceStoreModule = require('../../../source/renderer/app/stores/GovernanceStore');

const GovernanceStore =
  governanceStoreModule.default as typeof GovernanceStoreClass;
const { GovernanceRefreshState } = governanceStoreModule;

// cardano-wallet v2026-10-07, restarted after DRep A retired: the wallet still
// reports A as the vote it last sent, while its DRep registry, read from the
// ledger, no longer lists A.
const wallet = loadRecordedExchange('upstream', 'wallet-retired-drep');
const lookup = loadRecordedExchange('upstream', 'drep-lookup-retired');
const list = loadRecordedExchange('upstream', 'drep-list');
const summary = loadRecordedExchange('upstream', 'drep-summary');

const flush = () => new Promise((resolve) => realSetImmediate(resolve));

const currentDRepId = (): string => {
  global.cip30Enabled = false;
  const current = _createWalletFromServerData(
    JSON.parse(wallet.response.body)
  ).currentDRep;
  if (current?.kind !== 'drep') throw new Error('expected a DRep vote');
  return current.drep.cip129 ?? current.drep.raw;
};

const makeStore = (ada: unknown): GovernanceStoreClass => {
  const store = new GovernanceStore(
    {
      ada,
      localStorage: {
        getDRepFavorites: jest.fn().mockResolvedValue([]),
        setDRepFavorites: jest.fn().mockResolvedValue(undefined),
      },
    } as any,
    {} as any,
    {} as any
  );
  store.configure({
    networkStatus: {
      localTip: { epoch: list.chain?.epoch ?? 0 },
      isNodeInSync: true,
    },
    router: { location: { pathname: '/governance' } },
  } as any);
  return store;
};

describe('GovernanceStore with a wallet whose DRep retired', () => {
  let replay: WalletReplay | null = null;
  afterEach(async () => {
    jest.useRealTimers();
    await replay?.close();
    replay = null;
  });

  const replayWallet = async (lookupReply: ReplayRoute['reply']) => {
    const drepId = currentDRepId();
    replay = await startWalletReplay([
      {
        method: 'GET',
        path: new RegExp(`^/v2/dreps/${drepId}$`),
        reply: lookupReply,
      },
      { method: 'GET', path: /^\/v2\/dreps\/summary$/, reply: summary },
      { method: 'GET', path: /^\/v2\/dreps$/, reply: list },
    ]);
    const store = makeStore(new AdaApi(false, replay.config));
    // The delegation form adds the wallet's current DRep to the favourites.
    store.toggleFavorite(drepId);
    return { store, drepId };
  };

  const lookupsOf = (drepId: string) =>
    replay.received.filter(({ path }) => path === `/v2/dreps/${drepId}`);

  it('replays a wallet that names the DRep the registry answers 404 for', () => {
    expect(lookup.request.path).toBe(`/v2/dreps/${currentDRepId()}`);
    expect(lookup.response.status).toBe(404);
    expect(JSON.parse(lookup.response.body)).toMatchObject({
      code: 'not_found',
    });
  });

  knownDefect(
    'looks up a DRep the wallet registry does not list at most once',
    'a failed lookup is not recorded, so every pass sends it again',
    async () => {
      const { store, drepId } = await replayWallet(lookup);
      // The directory resolves favourites when it opens; the wallets page
      // resolves every wallet's DRep each time it updates.
      await store.ensureFavorites();
      for (let pass = 0; pass < 10; pass += 1) {
        await store.ensureDRep(drepId);
      }
      expect(lookupsOf(drepId)).toHaveLength(1);
    }
  );

  knownDefect(
    'keeps one lookup of a DRep in flight while the wallet has not answered',
    'every call sends another request',
    async () => {
      const { store, drepId } = await replayWallet('hold');
      for (let pass = 0; pass < 5; pass += 1) {
        store.ensureDRep(drepId);
      }
      await new Promise((resolve) => setTimeout(resolve, 200));
      expect(lookupsOf(drepId)).toHaveLength(1);
    }
  );

  it('loads the directory list while the lookup of that DRep fails', async () => {
    const { store, drepId } = await replayWallet(lookup);
    await Promise.all([store.ensureFavorites(), store.refresh()]);
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    expect(store.allDReps.length).toBe(JSON.parse(list.response.body).length);
    expect(store.allDReps.map(({ drepId: id }) => id)).not.toContain(drepId);
    expect(store.lookupDRep(drepId)).toBeNull();
  });

  it('loads the directory list while the lookup of that DRep is unanswered', async () => {
    const { store } = await replayWallet('hold');
    store.ensureFavorites();
    await store.refresh();
    expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
  });

  // Longer than any wait a list load should allow. The test fixes that a load
  // which never answers ends, not how long it may take.
  const LIST_WAIT_BOUND_MS = 10 * 60 * 1000;

  knownDefect(
    'starts a new list load on Refresh after one that never answers',
    'a list load has no time limit and Refresh returns while one is outstanding',
    async () => {
      jest.useFakeTimers('modern');
      const listDReps = jest
        .fn()
        .mockReturnValueOnce(new Promise(() => {}))
        .mockResolvedValue(JSON.parse(list.response.body));
      const store = makeStore({
        listDReps,
        getDRep: jest.fn(),
        getDRepSummary: jest
          .fn()
          .mockResolvedValue(JSON.parse(summary.response.body)),
      });
      store.refresh();
      await flush();
      jest.advanceTimersByTime(LIST_WAIT_BOUND_MS);
      await flush();
      store.refresh();
      await flush();
      expect(listDReps).toHaveBeenCalledTimes(2);
      expect(store.refreshState).toBe(GovernanceRefreshState.Loaded);
    }
  );
});

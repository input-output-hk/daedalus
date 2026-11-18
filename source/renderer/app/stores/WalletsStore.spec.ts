import WalletsStore from './WalletsStore';
import type Wallet from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { StoresMap } from './index';
import type { AnalyticsTracker } from '../analytics';
import { WalletSyncStateStatuses } from '../domains/Wallet';

const wallet = (id: string) =>
  ({
    id,
    isLegacy: false,
    isRestoring: false,
    isNotResponding: false,
  }) as Wallet;

const createStore = (wallets: Wallet[], currentRoute: string) => {
  const apiMethod = jest.fn();
  const apiNamespace = new Proxy({}, { get: () => apiMethod });
  const api = new Proxy({}, { get: () => apiNamespace });
  const store = new WalletsStore(api as any, {} as any, {} as any);
  const app = { currentRoute };

  store.configure({ app, addresses: { lastGeneratedAddress: null } } as any);
  store.walletsRequest.result = wallets;
  store._setActiveWallet = jest.fn(({ walletId }) => {
    store.active = wallets.find(({ id }) => id === walletId) || null;
  });
  store._unsetActiveWallet = jest.fn(() => {
    store.active = null;
  });
  store.goToWalletRoute = jest.fn();

  return { app, store };
};

describe('WalletsStore dApp routes', () => {
  const originalCapability = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCapability;
  });

  it('exposes no dApp wallet authority outside pilot packages', () => {
    global.cip30Enabled = false;
    const active = wallet('wallet-a');
    const { store } = createStore([active], '/wallets/wallet-a/summary');
    store.active = active;
    expect(store.eligibleDappWallets).toEqual([]);
    expect(store.activeDappWallet).toBeNull();
  });

  it('does not retarget a direct invalid dApp hash', () => {
    const { store } = createStore([wallet('wallet-a')], '/apps/missing');

    store._updateActiveWalletOnRouteChanges();

    expect(store._unsetActiveWallet).toHaveBeenCalledTimes(1);
    expect(store._setActiveWallet).not.toHaveBeenCalled();
    expect(store.goToWalletRoute).not.toHaveBeenCalled();
  });

  it('selects the wallet named by each dApp route', () => {
    const { app, store } = createStore(
      [wallet('wallet-a'), wallet('wallet-b')],
      '/apps/wallet-a'
    );

    store._updateActiveWalletOnRouteChanges();
    app.currentRoute = '/apps/wallet-b';
    store._updateActiveWalletOnRouteChanges();

    expect(store._setActiveWallet).toHaveBeenNthCalledWith(1, {
      walletId: 'wallet-a',
    });
    expect(store._setActiveWallet).toHaveBeenNthCalledWith(2, {
      walletId: 'wallet-b',
    });
    expect(store.goToWalletRoute).not.toHaveBeenCalled();
  });

  it.each([
    ['Byron', { isLegacy: true }],
    ['restoring', { isRestoring: true }],
    ['nonresponding', { isNotResponding: true }],
  ])('blocks %s wallets from dApp launch', (_state, override) => {
    const active = Object.assign(wallet('wallet-a'), override);
    const { store } = createStore([active], '/apps/wallet-a');
    store.active = active;

    expect(store.activeDappWallet).toBeNull();
  });

  it('blocks a deleted wallet from dApp launch', () => {
    const { store } = createStore([], '/apps/wallet-a');
    store.active = null;

    expect(store.activeDappWallet).toBeNull();
  });

  it('refreshes and removes an upstream wallet without connection services', async () => {
    global.cip30Enabled = false;
    const upstreamWallet = Object.assign(wallet('wallet-a'), {
      syncState: { status: WalletSyncStateStatuses.READY },
    });
    let backendWallets = [upstreamWallet];
    // Only backend and unrelated UI effects are replaced; refresh/delete requests run normally.
    const api = {
      ada: {
        getWallets: async () => backendWallets,
        deleteWallet: async () => {
          backendWallets = [];
          return true;
        },
      },
    } as unknown as Api;
    const trigger = jest.fn();
    const actions = {
      walletsLocal: {
        refreshWalletsLocalData: { trigger },
        unsetWalletLocalData: { trigger },
      },
      wallets: { refreshWalletsDataSuccess: { trigger } },
      router: { goToRoute: { trigger } },
      dialogs: { closeActiveDialog: { trigger } },
    } as unknown as ActionsMap;
    const analytics = { sendEvent: jest.fn() } as unknown as AnalyticsTracker;
    const store = new WalletsStore(api, actions, analytics);
    // Deliberately omit dapp: disabled packages cannot depend on its IPC lifecycle.
    const stores = {
      networkStatus: { isConnected: true },
      addresses: {
        _getAddressesAllRequest: jest.fn(),
        _refreshAddresses: jest.fn(),
      },
      transactions: {
        _getTransactionsRecentRequest: jest.fn(),
        _getTransactionsAllRequest: jest.fn(),
        _getWithdrawalsRequest: jest.fn(),
        _refreshTransactionData: jest.fn(),
      },
      hardwareWallets: { _unsetHardwareWalletLocalData: jest.fn() },
    } as unknown as StoresMap;
    store.configure(stores);
    await store.refreshWalletsData();
    expect(store.all.map(({ id }) => id)).toEqual(['wallet-a']);
    await store._deleteWallet({ walletId: 'wallet-a', isLegacy: false });
    expect(store.all).toEqual([]);
    expect(store.isDeleting).toBe(false);
    expect(store.active).toBeNull();
  });
});

describe('WalletsStore wallet polling', () => {
  const originalCapability = global.cip30Enabled;
  const syncedWallet = Object.assign(wallet('wallet-a'), {
    syncState: { status: WalletSyncStateStatuses.READY },
  });
  const restoredWallet = Object.assign(wallet('wallet-b'), {
    syncState: { status: WalletSyncStateStatuses.RESTORING },
  });
  let store: WalletsStore;

  const flush = () =>
    new Promise((resolve) => {
      setTimeout(resolve, 0);
    });

  const createPollingStore = () => {
    let finishRestore: (restored: Wallet) => void;
    const restoreResult = new Promise<Wallet>((resolve) => {
      finishRestore = resolve;
    });
    const getWallets = jest.fn(async () => [syncedWallet]);
    const restoreWallet = jest.fn(() => restoreResult);
    const ada = new Proxy(
      { getWallets, restoreWallet },
      { get: (methods, name) => methods[name] || jest.fn() }
    );
    // Every action is a stub, so setup() can attach its listeners
    const action = { listen: jest.fn(), trigger: jest.fn() };
    const actionNamespace = new Proxy({}, { get: () => action });
    const actions = new Proxy(
      {},
      { get: () => actionNamespace }
    ) as unknown as ActionsMap;
    const analytics = { sendEvent: jest.fn() } as unknown as AnalyticsTracker;
    store = new WalletsStore({ ada } as unknown as Api, actions, analytics);
    store.configure({
      app: { currentRoute: '/wallets/add' },
      networkStatus: { isConnected: true },
      addresses: {
        lastGeneratedAddress: null,
        _getAddressesAllRequest: jest.fn(),
        _refreshAddresses: jest.fn(),
      },
      transactions: {
        _getTransactionsRecentRequest: jest.fn(),
        _getTransactionsAllRequest: jest.fn(),
        _getWithdrawalsRequest: jest.fn(),
        _refreshTransactionData: jest.fn(),
      },
    } as unknown as StoresMap);
    store.initialize();

    return { finishRestore, getWallets, restoreWallet };
  };

  beforeEach(() => {
    global.cip30Enabled = false;
    // The test drives polling itself, so setup() starts no interval
    jest.spyOn(global, 'setInterval').mockReturnValue(undefined);
  });
  afterEach(() => {
    store.teardown();
    jest.restoreAllMocks();
    global.cip30Enabled = originalCapability;
  });

  it('requests the wallet list after a reconnection while the restored wallet dialog is open', async () => {
    const { finishRestore, getWallets } = createPollingStore();
    await store.refreshWalletsData();
    store._restoreWalletBegin();
    finishRestore(restoredWallet);
    await store._restore();
    expect(store.restoreWalletStep).toBe(3);
    getWallets.mockClear();

    // The connection to the wallet backend is restored
    store.resetWalletsData();
    await store._pollRefresh();

    expect(getWallets).toHaveBeenCalledTimes(1);
    expect(store.hasLoadedWallets).toBe(true);
  });

  it('resumes polling after a reconnection once the restore request has ended', async () => {
    const { finishRestore, getWallets, restoreWallet } = createPollingStore();
    await store.refreshWalletsData();
    store._restoreWalletBegin();
    const restoring = store._restore();
    await flush();
    expect(restoreWallet).toHaveBeenCalledTimes(1);
    getWallets.mockClear();

    store.resetWalletsData();
    await store._pollRefresh();
    expect(getWallets).not.toHaveBeenCalled();

    finishRestore(restoredWallet);
    await restoring;
    await store._pollRefresh();
    expect(getWallets).toHaveBeenCalledTimes(1);
  });

  it('keeps polling paused while the restored wallet dialog is open until it is closed', async () => {
    const { finishRestore, getWallets } = createPollingStore();
    await store.refreshWalletsData();
    store._restoreWalletBegin();
    finishRestore(restoredWallet);
    await store._restore();
    getWallets.mockClear();

    await store._pollRefresh();
    expect(getWallets).not.toHaveBeenCalled();

    await store._restoreWalletEnd();
    expect(getWallets).toHaveBeenCalledTimes(1);
    expect(store.restoreWalletStep).toBeNull();
  });
});

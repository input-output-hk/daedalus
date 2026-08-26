import WalletsStore from './WalletsStore';
import type Wallet from '../domains/Wallet';

const wallet = (id: string) =>
  ({
    id,
    isLegacy: false,
    isRestoring: false,
    isNotResponding: false,
  } as Wallet);

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
  it('does not retarget a direct invalid dApp hash', () => {
    const { store } = createStore(
      [wallet('wallet-a')],
      '/wallets/missing/dapps'
    );

    store._updateActiveWalletOnRouteChanges();

    expect(store._unsetActiveWallet).toHaveBeenCalledTimes(1);
    expect(store._setActiveWallet).not.toHaveBeenCalled();
    expect(store.goToWalletRoute).not.toHaveBeenCalled();
  });

  it('selects the wallet named by each dApp route', () => {
    const { app, store } = createStore(
      [wallet('wallet-a'), wallet('wallet-b')],
      '/wallets/wallet-a/dapps'
    );

    store._updateActiveWalletOnRouteChanges();
    app.currentRoute = '/wallets/wallet-b/dapps';
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
    const { store } = createStore([active], '/wallets/wallet-a/dapps');
    store.active = active;

    expect(store.activeDappWallet).toBeNull();
  });

  it('blocks a deleted wallet from dApp launch', () => {
    const { store } = createStore([], '/wallets/wallet-a/dapps');
    store.active = null;

    expect(store.activeDappWallet).toBeNull();
  });
});

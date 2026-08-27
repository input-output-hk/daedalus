import DappStore from './DappStore';
import {
  bindDappBrowserState,
  dappBrowserStatusChannel,
  openDappBrowserChannel,
} from '../ipc/dappBrowser';

jest.mock('../ipc/dappBrowser', () => ({
  bindDappBrowserState: jest.fn(() => jest.fn()),
  closeDappBrowserChannel: { request: jest.fn() },
  dappBrowserStatusChannel: { request: jest.fn() },
  openDappBrowserChannel: { request: jest.fn() },
}));

const statusRequest = dappBrowserStatusChannel.request as jest.Mock;
const openRequest = openDappBrowserChannel.request as jest.Mock;
const bindState = bindDappBrowserState as jest.Mock;

const createStore = () => {
  const store = new DappStore(null as any, null as any, null as any);
  store.configure({
    wallets: { activeDappWallet: { id: 'wallet-a' } },
    networkStatus: { isSynced: true },
  } as any);
  return store;
};

describe('DappStore', () => {
  beforeEach(() => jest.clearAllMocks());

  it('ignores status replies and state events from a retired lifecycle', async () => {
    let resolveStatus!: (value: any) => void;
    statusRequest.mockReturnValue(
      new Promise((resolve) => {
        resolveStatus = resolve;
      })
    );
    const store = createStore();
    store.setup();
    const receiveState = bindState.mock.calls[0][0];
    store.teardown();
    resolveStatus({ catalogAvailable: true, isOpen: true });
    receiveState(true);
    await Promise.resolve();

    expect(store.catalogAvailable).toBe(false);
    expect(store.guestOpen).toBe(false);
  });

  it('uses only an opaque ID and local name after main reports availability', async () => {
    statusRequest.mockResolvedValue({ catalogAvailable: true, isOpen: false });
    openRequest.mockResolvedValue(undefined);
    const store = createStore();
    store.setup();
    await Promise.resolve();
    await store.launch('catalog-id', 'Localized name');

    expect(openRequest).toHaveBeenCalledWith({
      catalogId: 'catalog-id',
      localName: 'Localized name',
    });
  });

  it('short-circuits launch while preferred catalog is unavailable', async () => {
    const store = createStore();
    await store.launch('catalog-id', 'Localized name');
    expect(openRequest).not.toHaveBeenCalled();
  });
});

import DappStore from './DappStore';
import {
  bindDappBrowserState,
  dappBrowserStatusChannel,
  openDappBrowserChannel,
} from '../ipc/dappBrowser';
import { dappConnectionsChannel } from '../ipc/dappConnections';

jest.mock('../ipc/dappBrowser', () => ({
  bindDappBrowserState: jest.fn(() => jest.fn()),
  closeDappBrowserChannel: { request: jest.fn() },
  dappBrowserStatusChannel: { request: jest.fn() },
  openDappBrowserChannel: { request: jest.fn() },
}));
jest.mock('../ipc/dappConnections', () => ({
  dappConnectionsChannel: { request: jest.fn() },
}));

const statusRequest = dappBrowserStatusChannel.request as jest.Mock;
const openRequest = openDappBrowserChannel.request as jest.Mock;
const bindState = bindDappBrowserState as jest.Mock;
const connectionsRequest = dappConnectionsChannel.request as jest.Mock;

const createStore = () => {
  const actions = { router: { goToRoute: { trigger: jest.fn() } } };
  const store = new DappStore(null as never, actions as never, null as never);
  store.configure({
    wallets: {
      activeDappWallet: { id: 'wallet-a' },
      eligibleDappWallets: [{ id: 'wallet-a' }],
      getWalletRoute: (id: string, page: string) => `/wallets/${id}/${page}`,
    },
    networkStatus: { isSynced: true },
  } as never);
  return { actions, store };
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
    const { store } = createStore();
    store.setup();
    const receiveState = bindState.mock.calls[0][0];
    store.teardown();
    resolveStatus({
      catalogAvailable: true,
      diagnosticsAvailable: true,
      isOpen: true,
    });
    receiveState(true);
    await Promise.resolve();

    expect(store.catalogAvailable).toBe(false);
    expect(store.guestOpen).toBe(false);
  });

  it('uses only an opaque ID and local name after main reports availability', async () => {
    statusRequest.mockResolvedValue({
      catalogAvailable: true,
      diagnosticsAvailable: false,
      isOpen: false,
    });
    openRequest.mockResolvedValue(undefined);
    const { store } = createStore();
    store.setup();
    await Promise.resolve();
    await store.launch('catalog-id', 'Localized name');

    expect(openRequest).toHaveBeenCalledWith({
      catalogId: 'catalog-id',
      localName: 'Localized name',
    });
  });

  it('short-circuits launch while preferred catalog is unavailable', async () => {
    const { store } = createStore();
    await store.launch('catalog-id', 'Localized name');
    expect(openRequest).not.toHaveBeenCalled();
  });

  it('stages diagnostics before routing without exposing the URL in the route', async () => {
    statusRequest.mockResolvedValue({
      catalogAvailable: false,
      diagnosticsAvailable: true,
      isOpen: false,
    });
    openRequest.mockResolvedValue(undefined);
    const { actions, store } = createStore();
    store.setup();
    await Promise.resolve();

    await store.launchDiagnostics(
      'https://example.com/private?value=1',
      'wallet-a',
      'Untrusted dApp'
    );

    expect(openRequest).toHaveBeenCalledWith({
      url: 'https://example.com/private?value=1',
      walletId: 'wallet-a',
      localName: 'Untrusted dApp',
    });
    expect(actions.router.goToRoute.trigger).toHaveBeenCalledWith({
      route: '/wallets/wallet-a/dapps',
    });
  });

  it('projects main-owned connections and sends exact revocation identities', async () => {
    const grant = {
      schemaVersion: 1 as const,
      origin: 'https://example.com',
      walletId: 'wallet-a',
      networkGenesis: 'genesis-a',
      networkMagic: 1,
      readScopes: ['connection', 'read', 'governance-key-disclosure'] as const,
      enabledExtensionScopes: [95],
      launch: { kind: 'diagnostics' as const },
      grantedAt: '2026-08-27T00:00:00.000Z',
    };
    connectionsRequest.mockResolvedValue({ corrupt: false, grants: [grant] });
    const { store } = createStore();

    await store.refreshConnections();
    await store.revokeConnectionScope(grant, 'governance-key-disclosure');

    expect(store.connections).toEqual([grant]);
    expect(connectionsRequest).toHaveBeenLastCalledWith({
      type: 'revoke-scope',
      identity: {
        origin: grant.origin,
        walletId: grant.walletId,
        networkGenesis: grant.networkGenesis,
        launch: grant.launch,
      },
      scope: 'governance-key-disclosure',
    });
  });

  it('prunes only when the authoritative wallet ID set changes', async () => {
    connectionsRequest.mockResolvedValue({ corrupt: false, grants: [] });
    const { store } = createStore();

    await store.pruneWalletConnections(['wallet-b', 'wallet-a']);
    await store.pruneWalletConnections(['wallet-a', 'wallet-b']);
    await store.pruneWalletConnections(['wallet-a']);

    expect(connectionsRequest).toHaveBeenCalledTimes(2);
    expect(connectionsRequest).toHaveBeenLastCalledWith({
      type: 'prune-wallets',
      walletIds: ['wallet-a'],
    });
  });
});

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
      consoleCaptureAvailable: true,
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
      consoleCaptureAvailable: true,
    });
    openRequest.mockResolvedValue(undefined);
    const { store } = createStore();
    store.setup();
    await Promise.resolve();
    await store.launch('catalog-id', 'Localized name');

    expect(openRequest).toHaveBeenCalledWith({
      catalogId: 'catalog-id',
      localName: 'Localized name',
      captureConsole: false,
    });
  });

  it('tracks only the catalog entry whose launch is pending', async () => {
    statusRequest.mockResolvedValue({
      catalogAvailable: true,
      diagnosticsAvailable: false,
      isOpen: false,
      consoleCaptureAvailable: true,
    });
    let resolveLaunch!: () => void;
    openRequest.mockReturnValue(
      new Promise<void>((resolve) => {
        resolveLaunch = resolve;
      })
    );
    const { store } = createStore();
    store.setup();
    await Promise.resolve();

    const launch = store.launch('catalog-id', 'Localized name');
    expect(store.launchingCatalogId).toBe('catalog-id');
    resolveLaunch();
    await launch;
    expect(store.launchingCatalogId).toBeNull();
  });

  it('consumes catalog rejection and clears pending state without opening a guest', async () => {
    openRequest.mockRejectedValue(new Error('Private IPC failure'));
    const { actions, store } = createStore();
    store.catalogAvailable = true;
    expect(store.catalogLaunchFailed).toBe(false);

    const launch = store.launch('catalog-id', 'Localized name');
    expect(store.isLaunching).toBe(true);
    expect(store.launchingCatalogId).toBe('catalog-id');
    await expect(launch).resolves.toBeUndefined();

    expect(store.catalogLaunchFailed).toBe(true);
    expect(store.isLaunching).toBe(false);
    expect(store.launchingCatalogId).toBeNull();
    expect(store.guestOpen).toBe(false);
    expect(actions.router.goToRoute.trigger).not.toHaveBeenCalled();
  });

  it('keeps catalog failure until a valid retry and clears it before IPC', async () => {
    openRequest.mockRejectedValueOnce(new Error('Launch failed'));
    const { store } = createStore();
    store.catalogAvailable = true;
    await store.launch('catalog-id', 'Localized name');

    store.catalogAvailable = false;
    await store.launch('catalog-id', 'Localized name');
    expect(store.catalogLaunchFailed).toBe(true);
    expect(openRequest).toHaveBeenCalledTimes(1);

    let resolveLaunch!: () => void;
    openRequest.mockImplementationOnce(() => {
      expect(store.catalogLaunchFailed).toBe(false);
      return new Promise<void>((resolve) => {
        resolveLaunch = resolve;
      });
    });
    store.catalogAvailable = true;
    const retry = store.launch('catalog-id', 'Localized name');
    expect(store.catalogLaunchFailed).toBe(false);
    expect(store.isLaunching).toBe(true);
    await store.launch('another-id', 'Another name');
    expect(openRequest).toHaveBeenCalledTimes(2);
    resolveLaunch();
    await retry;

    expect(store.catalogLaunchFailed).toBe(false);
    expect(store.isLaunching).toBe(false);
    expect(store.launchingCatalogId).toBeNull();
  });

  it('clears catalog failure on teardown', async () => {
    openRequest.mockRejectedValueOnce(new Error('Launch failed'));
    const { store } = createStore();
    store.catalogAvailable = true;
    await store.launch('catalog-id', 'Localized name');
    expect(store.catalogLaunchFailed).toBe(true);

    store.teardown();

    expect(store.catalogLaunchFailed).toBe(false);
    expect(store.isLaunching).toBe(false);
    expect(store.launchingCatalogId).toBeNull();
  });

  it('ignores catalog rejection from a retired lifecycle without clearing a new launch', async () => {
    statusRequest.mockResolvedValue({
      catalogAvailable: true,
      diagnosticsAvailable: false,
      isOpen: false,
      consoleCaptureAvailable: false,
    });
    let rejectOldLaunch!: (error: Error) => void;
    let resolveNewLaunch!: () => void;
    openRequest
      .mockImplementationOnce(
        () =>
          new Promise<void>((_, reject) => {
            rejectOldLaunch = reject;
          })
      )
      .mockImplementationOnce(
        () =>
          new Promise<void>((resolve) => {
            resolveNewLaunch = resolve;
          })
      );
    const { store } = createStore();
    store.setup();
    await Promise.resolve();
    const oldLaunch = store.launch('old-id', 'Old name');
    store.teardown();
    store.setup();
    await Promise.resolve();
    const newLaunch = store.launch('new-id', 'New name');

    rejectOldLaunch(new Error('Retired launch failed'));
    await expect(oldLaunch).resolves.toBeUndefined();

    expect(store.catalogLaunchFailed).toBe(false);
    expect(store.isLaunching).toBe(true);
    expect(store.launchingCatalogId).toBe('new-id');
    expect(store.guestOpen).toBe(false);
    resolveNewLaunch();
    await newLaunch;
    expect(store.isLaunching).toBe(false);
    expect(store.launchingCatalogId).toBeNull();
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
      consoleCaptureAvailable: true,
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
      captureConsole: false,
    });
    expect(actions.router.goToRoute.trigger).toHaveBeenCalledWith({
      route: '/apps/wallet-a',
    });
  });

  it.each([false, true])('preserves diagnostics rejection ownership with catalog failure %s', async (catalogLaunchFailed) => {
    const failure = new Error('Diagnostics launch failed');
    openRequest.mockRejectedValueOnce(failure);
    const { actions, store } = createStore();
    store.diagnosticsAvailable = true;
    store.catalogLaunchFailed = catalogLaunchFailed;

    const launch = store.launchDiagnostics(
      'https://example.com/private?value=1',
      'wallet-a',
      'Untrusted dApp'
    );
    expect(store.isLaunching).toBe(true);
    await expect(launch).rejects.toBe(failure);

    expect(store.isLaunching).toBe(false);
    expect(store.launchingCatalogId).toBeNull();
    expect(store.guestOpen).toBe(false);
    expect(store.catalogLaunchFailed).toBe(catalogLaunchFailed);
    expect(actions.router.goToRoute.trigger).not.toHaveBeenCalled();
  });

  it('captures console output only after an available diagnostics opt-in', async () => {
    statusRequest.mockResolvedValue({
      catalogAvailable: true,
      diagnosticsAvailable: true,
      consoleCaptureAvailable: true,
      isOpen: false,
    });
    openRequest.mockResolvedValue(undefined);
    const { store } = createStore();
    store.setup();
    await Promise.resolve();

    store.setConsoleCaptureEnabled(true);
    await store.launch('catalog-id', 'Localized name');

    expect(openRequest).toHaveBeenCalledWith({
      catalogId: 'catalog-id',
      localName: 'Localized name',
      captureConsole: true,
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

import type { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappCatalogEntry } from '../dapp/dappCatalog';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';
import { DappBrowserController } from './dappBrowser';

jest.mock('../config', () => ({
  dappLaunchPolicy: { allows: () => false },
  launcherConfig: { nodeConfig: { network: { genesisHash: 'genesis' } } },
}));
jest.mock('../environment', () => ({
  environment: { isDev: false },
}));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onRequest: jest.fn() })),
}));

const entry: DappCatalogEntry = {
  id: 'example',
  nameMessageId: 'dapp.example.name',
  descriptionMessageId: 'dapp.example.description',
  iconAsset: 'example.svg',
  entryUrlByNetworkGenesis: { genesis: 'https://example.com/app' },
  canonicalOrigin: 'https://example.com',
  allowedResourceOrigins: [],
  supportedWalletKinds: ['shelley'],
  supportedExtensions: [],
};

const enabledPolicy = (preferred = true, diagnostics = true) =>
  new DappLaunchPolicy({
    revision: 1,
    globalEnabled: true,
    preferredCatalogEnabled: preferred,
    diagnosticsEnabled: diagnostics,
    cip104Revision: 0,
    cip142Revision: 0,
  });

describe('DappBrowserController', () => {
  const makeManager = () => ({
    isOpen: false,
    launch: jest.fn(() => Promise.resolve()),
    launchDiagnostics: jest.fn(() => Promise.resolve()),
    close: jest.fn(() => Promise.resolve()),
  });

  it('stages diagnostics until the exact wallet route commits and consumes it once', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy()
    );
    let navigate:
      | ((_event: unknown, url: string, isMainFrame: boolean) => void)
      | undefined;
    controller.observeWindow(({
      webContents: {
        on: jest.fn((name, callback) => {
          if (name === 'did-navigate-in-page') navigate = callback;
        }),
        once: jest.fn(),
        getURL: jest.fn(),
      },
    } as unknown) as Electron.BrowserWindow);

    await controller.open({
      url: 'https://example.com/app',
      walletId: 'wallet-a',
      localName: 'Untrusted dApp',
    });
    expect(manager.launchDiagnostics).not.toHaveBeenCalled();

    navigate?.({}, 'file:///app/index.html#/wallets/wallet-a/dapps', true);
    await Promise.resolve();

    expect(manager.launchDiagnostics).toHaveBeenCalledTimes(1);
    expect(
      manager.launchDiagnostics
    ).toHaveBeenCalledWith(
      'https://example.com/app',
      'https://example.com',
      'Untrusted dApp',
      { allowHttpLoopback: false }
    );
  });

  it('rejects diagnostics independently without affecting preferred launch', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(true, false),
      [entry]
    );
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/wallets/wallet-a/dapps'
    );

    await expect(
      controller.open({
        url: 'https://example.com',
        walletId: 'wallet-a',
        localName: 'Untrusted dApp',
      })
    ).rejects.toThrow('DApp launch is disabled');
    await controller.open({ catalogId: 'example', localName: 'Example' });
    expect(manager.launch).toHaveBeenCalledWith(entry, 'genesis', 'Example');
    expect(manager.launchDiagnostics).not.toHaveBeenCalled();
  });
  it('exposes preferred availability without enabling diagnostics or requiring an entry', () => {
    const manager = makeManager();
    const preferred = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(true, false),
      []
    );
    const diagnosticsOnly = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(false, true),
      [entry]
    );

    expect(preferred.status).toEqual({
      isOpen: false,
      catalogAvailable: true,
      diagnosticsAvailable: false,
    });
    expect(diagnosticsOnly.status).toEqual({
      isOpen: false,
      catalogAvailable: false,
      diagnosticsAvailable: true,
    });
  });

  it('resolves a preferred catalog ID only from the injected main catalog', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(),
      [entry]
    );
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/wallets/wallet-a/dapps'
    );

    await controller.open({ catalogId: 'example', localName: 'Example' });

    expect(manager.launch).toHaveBeenCalledWith(entry, 'genesis', 'Example');
    await expect(
      controller.open({ catalogId: 'unknown', localName: 'Unknown' })
    ).rejects.toThrow('Unknown dApp catalog entry');
  });

  it('reports state only after a successful launch and when explicitly closed', async () => {
    const manager = makeManager();
    const state = jest.fn();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(),
      [entry],
      state
    );
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/wallets/wallet-a/dapps'
    );

    await controller.open({ catalogId: 'example', localName: 'Example' });
    await controller.close();

    expect(state).toHaveBeenNthCalledWith(1, true);
    expect(state).toHaveBeenNthCalledWith(2, false);
  });

  it('consumes a pending diagnostics launch on a wrong-wallet route', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy()
    );
    let navigate:
      | ((_event: unknown, url: string, isMainFrame: boolean) => void)
      | undefined;
    controller.observeWindow(({
      webContents: {
        on: jest.fn((name, callback) => {
          if (name === 'did-navigate-in-page') navigate = callback;
        }),
        once: jest.fn(),
        getURL: jest.fn(),
      },
    } as unknown) as Electron.BrowserWindow);
    await controller.open({
      url: 'https://example.com',
      walletId: 'wallet-a',
      localName: 'Untrusted dApp',
    });

    navigate?.({}, 'file:///app/index.html#/wallets/wallet-b/dapps', true);
    await Promise.resolve();
    navigate?.({}, 'file:///app/index.html#/wallets/wallet-a/dapps', true);
    await Promise.resolve();

    expect(manager.launchDiagnostics).not.toHaveBeenCalled();
  });
});

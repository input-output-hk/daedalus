import { dappCatalog } from '../../common/config/dappCatalog';
import type { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappCatalogEntry } from '../dapp/dappCatalog';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';
import {
  DappBrowserController,
  isDappConsoleCaptureSupported,
} from './dappBrowser';

jest.mock('../config', () => ({
  dappLaunchPolicy: { allows: () => false },
  launcherConfig: {
    isFlight: false,
    nodeConfig: { network: { genesisHash: 'genesis' } },
  },
}));
jest.mock('../environment', () => ({
  environment: { isDev: false, network: 'preprod' },
}));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onRequest: jest.fn() })),
}));
jest.mock('../windows/windowBounds', () => ({
  restoreSavedWindowBounds: jest.fn(),
  saveWindowBoundsOnSizeAndPositionChange: jest.fn(),
}));

const entry: DappCatalogEntry = {
  id: 'example',
  availableIn: ['preprod'],
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
    hardwareConnectorEnabled: true,
  });

describe('DappBrowserController', () => {
  const makeManager = () => ({
    isOpen: false,
    launch: jest.fn(() => Promise.resolve()),
    launchDiagnostics: jest.fn(() => Promise.resolve()),
    close: jest.fn(() => Promise.resolve()),
  });
  const { launcherConfig } = jest.requireMock('../config');
  const { environment } = jest.requireMock('../environment');

  afterEach(() => {
    launcherConfig.isFlight = false;
    environment.network = 'preprod';
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
      captureConsole: false,
    });
    expect(manager.launchDiagnostics).not.toHaveBeenCalled();

    navigate?.({}, 'file:///app/index.html#/apps/wallet-a', true);
    await Promise.resolve();

    expect(manager.launchDiagnostics).toHaveBeenCalledTimes(1);
    expect(manager.launchDiagnostics).toHaveBeenCalledWith(
      'https://example.com/app',
      'https://example.com',
      'Untrusted dApp',
      { allowHttpLoopback: false },
      false
    );
  });

  it('revokes the guest before a trusted renderer reload', () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy()
    );
    let startNavigation:
      | ((_event: { isMainFrame: boolean; isSameDocument: boolean }) => void)
      | undefined;
    controller.observeWindow(({
      webContents: {
        on: jest.fn((name, callback) => {
          if (name === 'did-start-navigation') startNavigation = callback;
        }),
        once: jest.fn(),
        getURL: jest.fn(),
      },
    } as unknown) as Electron.BrowserWindow);
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/apps/wallet-a'
    );

    startNavigation?.({ isMainFrame: true, isSameDocument: false });

    expect(controller.routeLease.current).toBeNull();
    expect(manager.close).toHaveBeenCalledWith('route-changed');
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
      'file:///app/index.html#/apps/wallet-a'
    );

    await expect(
      controller.open({
        url: 'https://example.com',
        walletId: 'wallet-a',
        localName: 'Untrusted dApp',
        captureConsole: false,
      })
    ).rejects.toThrow('DApp launch is disabled');
    await controller.open({
      catalogId: 'example',
      localName: 'Example',
      captureConsole: false,
    });
    expect(manager.launch).toHaveBeenCalledWith(
      entry,
      'genesis',
      'Example',
      false
    );
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
      consoleCaptureAvailable: true,
    });
    expect(diagnosticsOnly.status).toEqual({
      isOpen: false,
      catalogAvailable: false,
      diagnosticsAvailable: true,
      consoleCaptureAvailable: true,
    });
  });

  it('limits console capture to Flight, Preprod, and Preview builds', async () => {
    expect(isDappConsoleCaptureSupported('mainnet', true)).toBe(true);
    expect(isDappConsoleCaptureSupported('preprod', false)).toBe(true);
    expect(isDappConsoleCaptureSupported('preview', false)).toBe(true);
    expect(isDappConsoleCaptureSupported('mainnet', false)).toBe(false);

    environment.network = 'mainnet';
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(),
      [entry]
    );
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/apps/wallet-a'
    );

    await expect(
      controller.open({
        catalogId: 'example',
        localName: 'Example',
        captureConsole: true,
      })
    ).rejects.toThrow('DApp console capture is unavailable');
    expect(manager.launch).not.toHaveBeenCalled();
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
      'file:///app/index.html#/apps/wallet-a'
    );

    await controller.open({
      catalogId: 'example',
      localName: 'Example',
      captureConsole: false,
    });

    expect(manager.launch).toHaveBeenCalledWith(
      entry,
      'genesis',
      'Example',
      false
    );
    await expect(
      controller.open({
        catalogId: 'unknown',
        localName: 'Unknown',
        captureConsole: false,
      })
    ).rejects.toThrow('Unknown dApp catalog entry');
  });

  it('rejects catalog entries hidden from the running variant', async () => {
    const manager = makeManager();
    const hidden = {
      ...entry,
      id: 'hidden',
      availableIn: ['mainnet'] as const,
    };
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(),
      [entry, hidden]
    );
    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/apps/wallet-a'
    );

    await expect(
      controller.open({
        catalogId: 'hidden',
        localName: 'Hidden',
        captureConsole: false,
      })
    ).rejects.toThrow('Unknown dApp catalog entry');
    expect(manager.launch).not.toHaveBeenCalled();
  });

  it('enforces bundled visibility for Preprod and normalized Mainnet Flight', async () => {
    const preprodManager = makeManager();
    const preprod = new DappBrowserController(
      (preprodManager as unknown) as DappBrowserManager,
      'preprod-genesis',
      enabledPolicy(),
      dappCatalog
    );
    preprod.routeLease.observeTrustedRoute(
      'file:///app/index.html#/apps/wallet-a'
    );
    await expect(
      preprod.open({
        catalogId: 'liqwid-finance',
        localName: 'Liqwid Finance',
        captureConsole: false,
      })
    ).rejects.toThrow('Unknown dApp catalog entry');
    expect(preprodManager.launch).not.toHaveBeenCalled();
    await preprod.open({
      catalogId: 'unfrack-it',
      localName: 'unfrack.it',
      captureConsole: false,
    });
    expect(preprodManager.launch).toHaveBeenCalledWith(
      dappCatalog[1],
      'preprod-genesis',
      'unfrack.it',
      false
    );

    environment.network = 'mainnet';
    launcherConfig.isFlight = true;
    const flightManager = makeManager();
    const flight = new DappBrowserController(
      (flightManager as unknown) as DappBrowserManager,
      'mainnet-genesis',
      enabledPolicy(),
      dappCatalog
    );
    flight.routeLease.observeTrustedRoute(
      'file:///app/index.html#/apps/wallet-a'
    );
    await flight.open({
      catalogId: 'liqwid-finance',
      localName: 'Liqwid Finance',
      captureConsole: false,
    });
    expect(flightManager.launch).toHaveBeenCalledWith(
      dappCatalog[0],
      'mainnet-genesis',
      'Liqwid Finance',
      false
    );
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
      'file:///app/index.html#/apps/wallet-a'
    );

    await controller.open({
      catalogId: 'example',
      localName: 'Example',
      captureConsole: false,
    });
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
      captureConsole: false,
    });

    navigate?.({}, 'file:///app/index.html#/apps/wallet-b', true);
    await Promise.resolve();
    navigate?.({}, 'file:///app/index.html#/apps/wallet-a', true);
    await Promise.resolve();

    expect(manager.launchDiagnostics).not.toHaveBeenCalled();
  });
});

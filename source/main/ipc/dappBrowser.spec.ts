import type { DappBrowserManager } from '../dapp/DappBrowserManager';
import type { DappCatalogEntry } from '../dapp/dappCatalog';
import { DappLaunchPolicy } from '../dapp/DappLaunchPolicy';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import { DappBrowserController } from './dappBrowser';

jest.mock('../config', () => ({
  dappLaunchPolicy: { allows: () => false },
  launcherConfig: { nodeConfig: { network: { genesisHash: 'genesis' } } },
}));
jest.mock('./lib/MainIpcChannel', () => ({
  MainIpcChannel: jest.fn(() => ({ onRequest: jest.fn() })),
}));

const entry: DappCatalogEntry = {
  id: 'example',
  nameMessageId: 'dapp.example.name',
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

const requireLease = (lease: DappRouteLease | null): DappRouteLease => {
  if (!lease) throw new Error('Expected route lease');
  return lease;
};

describe('DappBrowserController', () => {
  const makeManager = () => ({
    launch: jest.fn(() => Promise.resolve()),
    close: jest.fn(() => Promise.resolve()),
  });

  it('opens only a main-staged launch bound to the current lease', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy()
    );
    const lease = requireLease(
      controller.routeLease.observeTrustedRoute(
        'file:///app/index.html#/wallets/wallet-a/dapps'
      )
    );
    const launchId = controller.stageLaunch({
      lease,
      mode: 'preferred',
      entry,
      localName: 'Example',
    });

    await controller.open({ launchId, lease });

    expect(manager.launch).toHaveBeenCalledWith(entry, 'genesis', 'Example');
    await expect(controller.open({ launchId, lease })).rejects.toThrow(
      'Unknown dApp launch'
    );
  });

  it('rejects the disabled mode without affecting the other mode', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy(false, true)
    );
    const lease = requireLease(
      controller.routeLease.observeTrustedRoute(
        'file:///app/index.html#/wallets/wallet-a/dapps'
      )
    );
    const launchId = controller.stageLaunch({
      lease,
      mode: 'preferred',
      entry,
      localName: 'Example',
    });

    await expect(controller.open({ launchId, lease })).rejects.toThrow(
      'DApp launch is disabled'
    );
    expect(manager.launch).not.toHaveBeenCalled();
    expect(enabledPolicy(false, true).allows('diagnostics')).toBe(true);
  });

  it('closes the guest and clears staged launches when the route changes', async () => {
    const manager = makeManager();
    const controller = new DappBrowserController(
      (manager as unknown) as DappBrowserManager,
      'genesis',
      enabledPolicy()
    );
    const lease = requireLease(
      controller.routeLease.observeTrustedRoute(
        'file:///app/index.html#/wallets/wallet-a/dapps'
      )
    );
    const launchId = controller.stageLaunch({
      lease,
      mode: 'preferred',
      entry,
      localName: 'Example',
    });

    controller.routeLease.observeTrustedRoute(
      'file:///app/index.html#/wallets/wallet-b/dapps'
    );

    expect(manager.close).toHaveBeenCalledWith('route-changed');
    await expect(controller.open({ launchId, lease })).rejects.toThrow(
      'Unknown dApp launch'
    );
  });
});

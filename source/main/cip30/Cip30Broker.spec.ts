import fs from 'fs';
import os from 'os';
import path from 'path';
import type { IpcMainInvokeEvent } from 'electron';
import type {
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../common/cip30/executor';

import { CapabilityService } from './CapabilityService';
import { Cip30Broker, parseConfiguredNetwork } from './Cip30Broker';
import type { Cip30BrokerOptions } from './Cip30Broker';
import { Dispatcher } from './Dispatcher';
import { ExtensionRegistry } from './ExtensionRegistry';
import { GrantRepository } from './GrantRepository';
import { Negotiator } from './Negotiator';
import { SessionStore } from './SessionStore';
import type { DappGuestAuthority } from '../dapp/DappBrowserManager';
import type { DappRouteLease } from '../dapp/DappRouteLease';
import type { ConsentRequest } from './ConsentCoordinator';

jest.mock('../config', () => {
  const { DappLaunchPolicy } = jest.requireActual('../dapp/DappLaunchPolicy');
  return {
    dappLaunchPolicy: new DappLaunchPolicy({
      revision: 1,
      globalEnabled: true,
      preferredCatalogEnabled: true,
      diagnosticsEnabled: true,
      cip104Revision: 0,
      cip142Revision: 0,
    }),
    launcherConfig: {
      cluster: 'testnet',
      nodeConfig: {
        network: {
          genesisFile: '/tmp/genesis.json',
          genesisHash: '11'.repeat(32),
        },
      },
    },
    stateDirectoryPath: '/tmp',
  };
});
jest.mock('../ipc/dappBrowser', () => ({
  authenticateDappGuest: jest.fn(),
  getCurrentDappRouteLease: jest.fn(),
  setDappBrokerLifecycleRevoker: jest.fn(),
}));
jest.mock('../ipc/dappConsent', () => ({ consentCoordinator: {} }));
jest.mock('../ipc/cip30Wallet', () => ({
  executeCip30WalletRequest: jest.fn(),
}));

const network = {
  networkId: 0 as const,
  networkMagic: 42,
  genesisHash: '11'.repeat(32),
};
const lease: DappRouteLease = {
  walletId: 'wallet',
  routeEpoch: 7,
  networkGenesis: network.genesisHash,
};
const launch = {
  kind: 'catalog' as const,
  catalogEntryId: 'dex',
  catalogEntryIdentity: 'identity',
};
const event = {} as IpcMainInvokeEvent;
const request = (method: string, args: unknown[] = []) => ({ method, args });

const create = () => {
  const directory = fs.mkdtempSync(path.join(os.tmpdir(), 'cip30-broker-'));
  const currentLease: DappRouteLease | null = lease;
  let guestCurrent = true;
  const guest: DappGuestAuthority = {
    guestWebContentsId: 9,
    documentGeneration: 3,
    origin: 'https://dapp.test',
    launch,
    isCurrent: () => guestCurrent,
  };
  const registry = new ExtensionRegistry();
  const capabilities = new CapabilityService(registry);
  const sessions = new SessionStore();
  const negotiator = new Negotiator(registry, capabilities);
  const dispatch = jest.fn(async (_request, _authority, _context) => [
    { cip: 95 },
    { cip: 103 },
  ]);
  const dispatcher = ({ dispatch } as unknown) as Dispatcher;
  const executeWallet = jest.fn<
    Promise<Cip30WalletResponse>,
    [Cip30WalletRequest]
  >(async (_request) => ({
    status: 'fulfilled',
    operation: 'capabilities',
    value: {
      walletId: lease.walletId,
      walletName: 'Wallet',
      walletKind: 'shelley-software',
      network,
      backendApiVersion: 1,
      backendExtensions: [95, 103],
    },
  }));
  const consent = ({
    request: jest.fn(async (pending: ConsentRequest<unknown>) =>
      pending.execute(pending.payload, new AbortController().signal)
    ),
  } as unknown) as Cip30BrokerOptions['consent'];
  const options: Cip30BrokerOptions = {
    authenticate: () => guest,
    currentLease: () => currentLease,
    executeWallet,
    consent,
    grants: new GrantRepository(path.join(directory, 'grants.json')),
    sessions,
    registry,
    capabilities,
    negotiator,
    dispatcher,
    network,
    networkName: 'Testnet',
    sourceRevision: '22'.repeat(20),
    now: () => new Date('2026-08-27T00:00:00.000Z'),
    connectionId: () => 'connection',
  };
  return {
    broker: new Cip30Broker(options),
    options,
    consent,
    dispatch,
    executeWallet,
    sessions,
    setGuestCurrent: (value: boolean) => {
      guestCurrent = value;
    },
    cleanup: () => fs.rmSync(directory, { recursive: true, force: true }),
  };
};

describe('Cip30Broker', () => {
  it('authenticates before parsing or invoking trusted wallet work', async () => {
    const fixture = create();
    const broker = new Cip30Broker({
      ...fixture.options,
      authenticate: () => null,
    });

    await expect(broker.handle(event, { malicious: true })).resolves.toEqual({
      status: 'rejected',
      rejection: { type: 'api-error', value: { code: -3, info: 'Refused' } },
    });
    expect(fixture.executeWallet).not.toHaveBeenCalled();
    fixture.cleanup();
  });

  it('uses correlated consent once, persists the grant, and replaces live sessions', async () => {
    const fixture = create();
    const enable = request('provider.enable', [
      { extensions: [{ cip: 95 }, { cip: 103 }, { cip: 999 }] },
    ]);

    await expect(
      fixture.broker.handle(event, request('provider.isEnabled'))
    ).resolves.toEqual({ status: 'fulfilled', value: false });

    await expect(fixture.broker.handle(event, enable)).resolves.toMatchObject({
      status: 'fulfilled',
    });
    expect(fixture.consent.request).toHaveBeenCalledTimes(1);
    expect(
      (fixture.consent.request as jest.Mock).mock.calls[0][0].presentation.kind
    ).toBe('key-disclosure');
    expect(fixture.sessions.currentForGuest(9)?.enabledExtensions).toEqual([
      95,
      103,
    ]);

    await expect(
      fixture.broker.handle(event, request('provider.isEnabled'))
    ).resolves.toEqual({ status: 'fulfilled', value: true });
    await expect(fixture.broker.handle(event, enable)).resolves.toMatchObject({
      status: 'fulfilled',
    });
    expect(fixture.consent.request).toHaveBeenCalledTimes(1);
    expect(fixture.sessions.currentForGuest(9)?.connectionId).toBe(
      'connection'
    );

    await expect(
      fixture.broker.handle(event, request('api.getExtensions'))
    ).resolves.toEqual({
      status: 'fulfilled',
      value: [{ cip: 95 }, { cip: 103 }],
    });
    expect(fixture.dispatch).toHaveBeenCalledTimes(1);
    fixture.cleanup();
  });

  it('refuses future methods before backend work and maps stale authority to AccountChange', async () => {
    const fixture = create();
    await fixture.broker.handle(event, request('provider.enable'));
    fixture.executeWallet.mockClear();

    await expect(
      fixture.broker.handle(event, request('api.signTx', ['84a0a0f5f6']))
    ).resolves.toEqual({
      status: 'rejected',
      rejection: { type: 'api-error', value: { code: -3, info: 'Refused' } },
    });
    expect(fixture.executeWallet).not.toHaveBeenCalled();

    fixture.executeWallet.mockResolvedValueOnce({
      status: 'rejected',
      reason: 'unavailable',
    });
    await expect(
      fixture.broker.handle(event, request('api.getNetworkId'))
    ).resolves.toEqual({
      status: 'rejected',
      rejection: {
        type: 'api-error',
        value: { code: -2, info: 'Internal error' },
      },
    });
    expect(fixture.executeWallet).toHaveBeenCalledTimes(1);

    fixture.setGuestCurrent(false);
    await expect(
      fixture.broker.handle(event, request('api.getNetworkId'))
    ).resolves.toEqual({
      status: 'rejected',
      rejection: {
        type: 'api-error',
        value: { code: -4, info: 'Account changed' },
      },
    });
    fixture.cleanup();
  });

  it('suppresses a read result when guest authority changes in flight', async () => {
    const fixture = create();
    await fixture.broker.handle(event, request('provider.enable'));
    fixture.dispatch.mockClear();
    let finish!: () => void;
    const response = new Promise<{
      status: 'fulfilled';
      operation: 'capabilities';
      value: {
        walletId: string;
        walletName: string;
        walletKind: 'shelley-software';
        network: typeof network;
        backendApiVersion: 1;
        backendExtensions: number[];
      };
    }>((resolve) => {
      finish = () =>
        resolve({
          status: 'fulfilled',
          operation: 'capabilities',
          value: {
            walletId: lease.walletId,
            walletName: 'Wallet',
            walletKind: 'shelley-software',
            network,
            backendApiVersion: 1,
            backendExtensions: [95, 103],
          },
        });
    });
    fixture.executeWallet.mockImplementationOnce(() => response);

    const pending = fixture.broker.handle(event, request('api.getNetworkId'));
    fixture.setGuestCurrent(false);
    finish();

    await expect(pending).resolves.toEqual({
      status: 'rejected',
      rejection: {
        type: 'api-error',
        value: { code: -4, info: 'Account changed' },
      },
    });
    expect(fixture.dispatch).not.toHaveBeenCalled();
    fixture.cleanup();
  });

  it('derives the configured identity from Shelley or packaged Byron genesis', () => {
    expect(
      parseConfiguredNetwork(
        { networkMagic: 42 },
        'testnet',
        network.genesisHash
      )
    ).toEqual(network);
    expect(
      parseConfiguredNetwork(
        { protocolConsts: { protocolMagic: 764824073 } },
        'mainnet',
        network.genesisHash
      )
    ).toEqual({
      networkId: 1,
      networkMagic: 764824073,
      genesisHash: network.genesisHash,
    });
    expect(() =>
      parseConfiguredNetwork({}, 'testnet', network.genesisHash)
    ).toThrow('Invalid configured network identity');
  });
});

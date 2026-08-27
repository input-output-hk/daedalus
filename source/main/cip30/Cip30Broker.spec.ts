import { generateKeyPairSync, sign as signBytes } from 'crypto';
import { blake2b } from 'blakejs';
import fs from 'fs';
import os from 'os';
import path from 'path';
import type { IpcMainInvokeEvent } from 'electron';
import type {
  Cip30WalletRequest,
  Cip30WalletResponse,
} from '../../common/cip30/executor';
import { prepareCip8Request, serializeCip8 } from '../../common/cardano/cip8';
import {
  encodeCoseProtectedHeader,
  encodeCoseSignatureStructure,
} from '../../common/cardano/cose';

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

const createDataSignatureFixture = () => {
  const keys = generateKeyPairSync('ed25519');
  const publicDer = keys.publicKey.export({
    format: 'der',
    type: 'spki',
  }) as Buffer;
  const publicKey = publicDer.subarray(-32);
  const credential = Buffer.from(blake2b(publicKey, undefined, 28));
  const address = Buffer.concat([Buffer.from([0x60]), credential]).toString(
    'hex'
  );
  const payload = Buffer.from('Hello, Cardano', 'utf8').toString('hex');
  const expected = prepareCip8Request(address, payload, { networkId: 0 });
  const signature = signBytes(
    null,
    encodeCoseSignatureStructure(
      encodeCoseProtectedHeader(expected.protectedAddress),
      expected.payload
    ),
    keys.privateKey
  );
  const result = serializeCip8(expected, { publicKey, signature });
  return {
    address,
    payload,
    result,
    response: {
      revision: 1 as const,
      credential_kind: 'payment' as const,
      credential: credential.toString('hex'),
      cose_sign1: result.signature,
      cose_key: result.key,
    },
  };
};
const dataSignature = createDataSignatureFixture();

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
  const dispatcher = new Dispatcher(capabilities, sessions);
  const dispatch = jest
    .spyOn(dispatcher, 'dispatch')
    .mockResolvedValue([{ cip: 95 }, { cip: 103 }]);
  let walletKind: 'shelley-software' | 'ledger' = 'shelley-software';
  let signatureResponse = dataSignature.response;
  let signatureFailure: 'address-not-pk' | 'proof-generation' | null = null;
  const executeWallet = jest.fn<
    Promise<Cip30WalletResponse>,
    [Cip30WalletRequest]
  >(async (walletRequest) => {
    if (walletRequest.operation === 'sign-data')
      return signatureFailure
        ? {
            status: 'rejected',
            reason: signatureFailure,
          }
        : {
            status: 'fulfilled',
            operation: 'sign-data',
            value: signatureResponse,
          };
    return {
      status: 'fulfilled',
      operation: 'capabilities',
      value: {
        walletId: lease.walletId,
        walletName: 'Wallet',
        walletKind,
        network,
        backendApiVersion: 1,
        backendExtensions: [95, 103],
      },
    };
  });
  const consent = ({
    request: jest.fn(async (pending: ConsentRequest<unknown>) =>
      pending.execute(pending.payload, new AbortController().signal, 'secret')
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
    setWalletKind: (value: 'shelley-software' | 'ledger') => {
      walletKind = value;
    },
    tamperSignature: () => {
      signatureResponse = {
        ...signatureResponse,
        cose_sign1: '00',
      };
    },
    setSignatureFailure: (
      value: 'address-not-pk' | 'proof-generation' | null
    ) => {
      signatureFailure = value;
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

  it('requires fresh exact-byte consent and verifies software COSE before release', async () => {
    const fixture = create();
    await fixture.broker.handle(event, request('provider.enable'));
    (fixture.consent.request as jest.Mock).mockClear();
    fixture.executeWallet.mockClear();
    const signRequest = request('api.signData', [
      dataSignature.address,
      dataSignature.payload,
    ]);

    await expect(fixture.broker.handle(event, signRequest)).resolves.toEqual({
      status: 'fulfilled',
      value: dataSignature.result,
    });
    await expect(fixture.broker.handle(event, signRequest)).resolves.toEqual({
      status: 'fulfilled',
      value: dataSignature.result,
    });
    expect(fixture.consent.request).toHaveBeenCalledTimes(2);
    expect(
      (fixture.consent.request as jest.Mock).mock.calls[0][0].presentation
    ).toMatchObject({
      kind: 'data-sign',
      review: {
        address: dataSignature.address,
        payload: dataSignature.payload,
        credentialKind: 'payment',
        utf8Preview: 'Hello, Cardano',
      },
    });
    const calls = fixture.executeWallet.mock.calls
      .map(([value]) => value)
      .filter((value) => value.operation === 'sign-data');
    expect(calls).toHaveLength(2);
    expect(calls[0]).toMatchObject({
      address: dataSignature.address,
      payload: dataSignature.payload,
      passphrase: 'secret',
    });
    fixture.cleanup();
  });

  it('maps script, hardware, and invalid returned COSE without releasing data', async () => {
    const fixture = create();
    await fixture.broker.handle(event, request('provider.enable'));
    (fixture.consent.request as jest.Mock).mockClear();
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [`70${'11'.repeat(28)}`, ''])
      )
    ).resolves.toMatchObject({
      status: 'rejected',
      rejection: { type: 'data-sign-error', value: { code: 2 } },
    });
    expect(fixture.consent.request).not.toHaveBeenCalled();

    fixture.setWalletKind('ledger');
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [dataSignature.address, dataSignature.payload])
      )
    ).resolves.toMatchObject({
      status: 'rejected',
      rejection: { type: 'data-sign-error', value: { code: 1 } },
    });
    expect(fixture.consent.request).not.toHaveBeenCalled();

    fixture.setWalletKind('shelley-software');
    fixture.setSignatureFailure('address-not-pk');
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [dataSignature.address, dataSignature.payload])
      )
    ).resolves.toMatchObject({
      status: 'rejected',
      rejection: { type: 'data-sign-error', value: { code: 2 } },
    });
    fixture.setSignatureFailure('proof-generation');
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [dataSignature.address, dataSignature.payload])
      )
    ).resolves.toMatchObject({
      status: 'rejected',
      rejection: { type: 'data-sign-error', value: { code: 1 } },
    });
    fixture.setSignatureFailure(null);

    (fixture.consent.request as jest.Mock).mockRejectedValueOnce({
      type: 'data-sign-error',
      value: { code: 3, info: 'User declined' },
    });
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [dataSignature.address, dataSignature.payload])
      )
    ).resolves.toMatchObject({
      status: 'rejected',
      rejection: { type: 'data-sign-error', value: { code: 3 } },
    });

    fixture.tamperSignature();
    await expect(
      fixture.broker.handle(
        event,
        request('api.signData', [dataSignature.address, dataSignature.payload])
      )
    ).resolves.toEqual({
      status: 'rejected',
      rejection: {
        type: 'api-error',
        value: { code: -2, info: 'Internal error' },
      },
    });
    fixture.cleanup();
  });

  it.each([
    ['mainnet', 764824073, 1],
    ['preprod', 1, 0],
    ['preview', 2, 0],
    ['custom', 1097911063, 0],
  ])('derives configured %s network magic', (cluster, magic, networkId) => {
    expect(
      parseConfiguredNetwork(
        { protocolConsts: { protocolMagic: magic } },
        cluster,
        network.genesisHash
      )
    ).toEqual({
      networkId,
      networkMagic: magic,
      genesisHash: network.genesisHash,
    });
  });

  it('accepts Shelley genesis and rejects missing configured magic', () => {
    expect(
      parseConfiguredNetwork(
        { networkMagic: 42 },
        'testnet',
        network.genesisHash
      )
    ).toEqual(network);
    expect(() =>
      parseConfiguredNetwork({}, 'testnet', network.genesisHash)
    ).toThrow('Invalid configured network identity');
  });
});

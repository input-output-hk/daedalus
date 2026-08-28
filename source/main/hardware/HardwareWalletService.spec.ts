import { generateKeyPairSync, sign } from 'crypto';
import { blake2b } from 'blakejs';
import type { BrowserWindow } from 'electron';
import type AppAda from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type TransportNodeHid from '@ledgerhq/hw-transport-node-hid-noevents';
import TrezorConnect from '@trezor/connect';

import type { HardwareExactTransaction } from '../../common/types/hardware-wallets.types';
import { toExactTrezorSignTransactionRequest } from '../../common/hardware/trezorTransaction';

import type { HardwareWalletChannels } from '../ipc/createHardwareWalletIPCChannels';
import type { DeviceDetectionPayload } from '../ipc/hardwareWallets/ledger/deviceDetection/deviceDetection';
import { handleHardwareWalletRequests } from '../ipc/getHardwareWalletChannel';
import {
  HardwareWalletOperationCancelled,
  HardwareWalletService,
  LedgerServiceDependencies,
} from './HardwareWalletService';

jest.mock('@ledgerhq/hw-transport-node-hid-noevents', () => ({
  __esModule: true,
  default: { open: jest.fn(), list: jest.fn() },
  getDevices: jest.fn(),
}));

jest.mock('@cardano-foundation/ledgerjs-hw-app-cardano', () => ({
  __esModule: true,
  default: class {},
  utils: { bech32_encodeAddress: jest.fn(), buf_to_hex: jest.fn() },
}));

jest.mock('../../common/hardware/ledgerTransaction', () => ({
  toExactLedgerSignTransactionRequest: jest.fn(() => ({ tx: {} })),
}));
jest.mock('../../common/hardware/trezorTransaction', () => ({
  toExactTrezorSignTransactionRequest: jest.fn(() => ({})),
}));

jest.mock('@trezor/connect', () => ({
  __esModule: true,
  default: {
    removeAllListeners: jest.fn(),
    cancel: jest.fn(),
    on: jest.fn(),
    cardanoSignTransaction: jest.fn(),
  },
  DEVICE: { CONNECT: 'connect', DISCONNECT: 'disconnect', CHANGED: 'changed' },
  DEVICE_EVENT: 'device-event',
  TRANSPORT: { ERROR: 'transport-error' },
  TRANSPORT_EVENT: 'transport-event',
  UI: { REQUEST_PASSPHRASE: 'request-passphrase' },
  UI_EVENT: 'ui-event',
}));

const channelNames: Array<keyof HardwareWalletChannels> = [
  'getHardwareWalletTransportChannel',
  'getExtendedPublicKeyChannel',
  'getCardanoAdaAppChannel',
  'getHardwareWalletConnectionChannel',
  'signTransactionLedgerChannel',
  'signTransactionTrezorChannel',
  'resetTrezorActionChannel',
  'handleInitTrezorConnectChannel',
  'handleInitLedgerConnectChannel',
  'deriveXpubChannel',
  'deriveAddressChannel',
  'showAddressChannel',
  'waitForLedgerDevicesToConnectChannel',
];

const createChannels = () => {
  const handlers = new Map<string, (value?: unknown) => Promise<unknown>>();
  const send = jest.fn(() => Promise.resolve(undefined));
  const channelMap: Partial<Record<keyof HardwareWalletChannels, unknown>> = {};
  channelNames.forEach((name) => {
    channelMap[name] = {
      onRequest: (handler) => handlers.set(name, handler),
      send,
    };
  });
  const channels = channelMap as HardwareWalletChannels;
  return { channels, handlers, send };
};

const detectedDevice = {
  type: 'add',
  descriptor: 'descriptor',
  deviceModel: {
    id: 'nanoS',
    productName: 'Nano S',
    productIdMM: 1,
    legacyUsbProductId: 1,
    usbOnly: true,
    memorySize: 1,
    blockSize: 1,
  },
  device: {
    vendorId: 1,
    productId: 1,
    path: 'ledger-path',
    release: 1,
    interface: 1,
    product: 'Ledger',
  },
} as DeviceDetectionPayload;

const flush = async (): Promise<void> => {
  await Promise.resolve();
  await Promise.resolve();
};

describe('HardwareWalletService', () => {
  it('owns detection, transport cancellation, and late-result suppression', async () => {
    let onAdd: ((payload: DeviceDetectionPayload) => void) | undefined;
    const unsubscribe = jest.fn();
    const close = jest.fn(() => Promise.resolve());
    const transport = ({ close } as unknown) as TransportNodeHid;
    const connection = {} as AppAda;
    const dependencies: LedgerServiceDependencies = {
      open: jest.fn(() => Promise.resolve(transport)),
      list: jest.fn(() => Promise.resolve(['ledger-path'])),
      getDevices: jest.fn(() => [detectedDevice.device]),
      detect: jest.fn((add) => {
        onAdd = add;
        return unsubscribe;
      }),
      wait: jest.fn(() => Promise.resolve(detectedDevice)),
      createApp: jest.fn(() => connection),
    };
    const service = new HardwareWalletService(dependencies);
    const { channels, handlers, send } = createChannels();

    await service.register(channels);
    await handlers.get('handleInitLedgerConnectChannel')!();
    await handlers.get('handleInitLedgerConnectChannel')!();
    expect(dependencies.detect).toHaveBeenCalledTimes(1);

    onAdd!(detectedDevice);
    await flush();
    expect(send).toHaveBeenCalledWith(
      expect.objectContaining({
        disconnected: false,
        deviceType: 'ledger',
        path: 'ledger-path',
      }),
      expect.anything()
    );

    let release: (value: string) => void = () => undefined;
    const operation = service.withLedgerOperation(
      'ledger-path',
      () =>
        new Promise<string>((resolve) => {
          release = resolve;
        })
    );
    await service.cancelLedgerOperation('ledger-path');
    release('late');

    await expect(operation).rejects.toBeInstanceOf(
      HardwareWalletOperationCancelled
    );
    expect(close).toHaveBeenCalledTimes(1);

    onAdd!(detectedDevice);
    await flush();
    let rejectLate: (error: Error) => void = () => undefined;
    const failedOperation = service.withLedgerOperation(
      'ledger-path',
      () =>
        new Promise<string>((_resolve, reject) => {
          rejectLate = reject;
        })
    );
    await service.cancelLedgerOperation('ledger-path');
    rejectLate(new Error('late vendor error'));
    await expect(failedOperation).rejects.toBeInstanceOf(
      HardwareWalletOperationCancelled
    );
    expect(close).toHaveBeenCalledTimes(2);

    await service.dispose();
    await service.dispose();
    expect(unsubscribe).toHaveBeenCalledTimes(1);
    expect(TrezorConnect.cancel).toHaveBeenCalledTimes(1);
  });

  it('releases only exact Ledger witnesses', async () => {
    const keys = generateKeyPairSync('ed25519');
    const publicKey = (keys.publicKey.export({
      format: 'der',
      type: 'spki',
    }) as Buffer).subarray(-32);
    const keyHash = Buffer.from(blake2b(publicKey, undefined, 28)).toString(
      'hex'
    );
    const bodyHash = 'ab'.repeat(32);
    const path = [0x8000073c, 0x80000717, 0x80000000, 0, 0];
    const signature = sign(null, Buffer.from(bodyHash, 'hex'), keys.privateKey);
    const signTransaction = jest.fn(() =>
      Promise.resolve({
        txHashHex: bodyHash,
        witnesses: [
          {
            path,
            witnessSignatureHex: signature.toString('hex'),
          },
        ],
        auxiliaryDataSupplement: null,
      })
    );
    const getExtendedPublicKey = jest.fn(() =>
      Promise.resolve({
        publicKeyHex: publicKey.toString('hex'),
        chainCodeHex: '00'.repeat(32),
      })
    );
    const connection = ({
      signTransaction,
      getExtendedPublicKey,
    } as unknown) as AppAda;
    let onAdd: ((payload: DeviceDetectionPayload) => void) | undefined;
    const dependencies: LedgerServiceDependencies = {
      open: jest.fn(() =>
        Promise.resolve(({
          close: jest.fn(() => Promise.resolve()),
        } as unknown) as TransportNodeHid)
      ),
      list: jest.fn(() => Promise.resolve(['ledger-path'])),
      getDevices: jest.fn(() => [detectedDevice.device]),
      detect: jest.fn((add) => {
        onAdd = add;
        return jest.fn();
      }),
      wait: jest.fn(() => Promise.resolve(detectedDevice)),
      createApp: jest.fn(() => connection),
    };
    const service = new HardwareWalletService(dependencies);
    const { channels, handlers } = createChannels();
    await service.register(channels);
    await handlers.get('handleInitLedgerConnectChannel')!();
    onAdd!(detectedDevice);
    await flush();
    const exact = ({
      bodyHash,
      partialSign: false,
      signers: [{ keyHash, path }],
      witnesses: {
        requiredKeyHashes: [keyHash],
        preExistingKeyHashes: [],
        requestedDeviceKeyHashes: [keyHash],
        missingKeyHashes: [],
        unexpectedKeyHashes: [],
      },
    } as unknown) as HardwareExactTransaction;

    await expect(
      service.signExactLedgerTransaction('ledger-path', exact)
    ).resolves.toMatch(/^a10081825820/u);
    expect(signTransaction).toHaveBeenCalledTimes(1);
    expect(getExtendedPublicKey).toHaveBeenCalledWith({ path });

    for (const response of [
      { txHashHex: '00'.repeat(32), witnesses: [] },
      {
        txHashHex: bodyHash,
        witnesses: [
          { path: [...path, 1], witnessSignatureHex: '00'.repeat(64) },
        ],
      },
      { txHashHex: bodyHash, witnesses: [] },
    ]) {
      signTransaction.mockResolvedValueOnce({
        ...response,
        auxiliaryDataSupplement: null,
      });
      await expect(
        service.signExactLedgerTransaction('ledger-path', exact)
      ).rejects.toThrow();
    }

    getExtendedPublicKey.mockResolvedValueOnce({
      publicKeyHex: '00'.repeat(32),
      chainCodeHex: '00'.repeat(32),
    });
    await expect(
      service.signExactLedgerTransaction('ledger-path', exact)
    ).rejects.toThrow('unexpected public key');

    signTransaction.mockResolvedValueOnce({
      txHashHex: bodyHash,
      witnesses: [
        {
          path,
          witnessSignatureHex: '00'.repeat(64),
        },
      ],
      auxiliaryDataSupplement: null,
    });
    await expect(
      service.signExactLedgerTransaction('ledger-path', exact)
    ).rejects.toThrow();
  });

  it('releases only verified exact Shelley witnesses from Trezor', async () => {
    const keys = generateKeyPairSync('ed25519');
    const publicKey = (keys.publicKey.export({
      format: 'der',
      type: 'spki',
    }) as Buffer).subarray(-32);
    const keyHash = Buffer.from(blake2b(publicKey, undefined, 28)).toString(
      'hex'
    );
    const bodyHash = 'cd'.repeat(32);
    const signature = sign(
      null,
      Buffer.from(bodyHash, 'hex'),
      keys.privateKey
    ).toString('hex');
    const exact = ({
      bodyHash,
      partialSign: false,
      signers: [{ keyHash, path: [0x8000073c, 0x80000717, 0x80000000, 0, 0] }],
      witnesses: {
        requiredKeyHashes: [keyHash],
        preExistingKeyHashes: [],
        requestedDeviceKeyHashes: [keyHash],
        missingKeyHashes: [],
        unexpectedKeyHashes: [],
      },
    } as unknown) as HardwareExactTransaction;
    const signTransaction = TrezorConnect.cardanoSignTransaction as jest.Mock;
    signTransaction.mockResolvedValue({
      success: true,
      payload: {
        hash: bodyHash,
        witnesses: [{ type: 1, pubKey: publicKey.toString('hex'), signature }],
      },
    });
    const service = new HardwareWalletService();

    await expect(service.signExactTrezorTransaction(exact)).resolves.toMatch(
      /^a10081825820/u
    );

    for (const payload of [
      { hash: '00'.repeat(32), witnesses: [] },
      { hash: bodyHash, witnesses: [] },
      {
        hash: bodyHash,
        witnesses: [{ type: 0, pubKey: publicKey.toString('hex'), signature }],
      },
      {
        hash: bodyHash,
        witnesses: [{ type: 1, pubKey: '00'.repeat(31), signature }],
      },
      {
        hash: bodyHash,
        witnesses: [{ type: 1, pubKey: '00'.repeat(32), signature }],
      },
      {
        hash: bodyHash,
        witnesses: [
          { type: 1, pubKey: publicKey.toString('hex'), signature },
          { type: 1, pubKey: publicKey.toString('hex'), signature },
        ],
      },
      {
        hash: bodyHash,
        witnesses: [
          {
            type: 1,
            pubKey: publicKey.toString('hex'),
            signature: '00'.repeat(64),
          },
        ],
      },
      {
        hash: bodyHash,
        witnesses: [
          {
            type: 1,
            pubKey: publicKey.toString('hex'),
            signature,
            chainCode: '00'.repeat(32),
          },
        ],
      },
      {
        hash: bodyHash,
        witnesses: [{ type: 1, pubKey: publicKey.toString('hex'), signature }],
        auxiliaryDataSupplement: {
          type: 0,
          auxiliaryDataHash: '00'.repeat(32),
        },
      },
    ]) {
      signTransaction.mockResolvedValueOnce({ success: true, payload });
      await expect(service.signExactTrezorTransaction(exact)).rejects.toThrow();
    }
    signTransaction.mockResolvedValueOnce({ success: false, payload: {} });
    await expect(service.signExactTrezorTransaction(exact)).rejects.toThrow();

    (toExactTrezorSignTransactionRequest as jest.Mock).mockImplementationOnce(
      () => {
        throw new Error('preflight');
      }
    );
    const calls = signTransaction.mock.calls.length;
    await expect(service.signExactTrezorTransaction(exact)).rejects.toThrow(
      'preflight'
    );
    expect(signTransaction).toHaveBeenCalledTimes(calls);
  });
  it('keeps every legacy trusted channel behind one service registration', async () => {
    const { channels, handlers } = createChannels();

    const service = ({
      register: jest.fn(() => Promise.resolve()),
    } as unknown) as HardwareWalletService;

    await handleHardwareWalletRequests({} as BrowserWindow, channels, service);
    expect(service.register).toHaveBeenCalledWith(channels);

    const realService = new HardwareWalletService(({
      open: jest.fn(),
      list: jest.fn(),
      getDevices: jest.fn(),
      detect: jest.fn(() => jest.fn()),
      wait: jest.fn(),
      createApp: jest.fn(),
    } as unknown) as LedgerServiceDependencies);
    await realService.register(channels);
    expect([...handlers.keys()].sort()).toEqual(
      channelNames
        .filter((name) => name !== 'getHardwareWalletConnectionChannel')
        .sort()
    );
  });
});

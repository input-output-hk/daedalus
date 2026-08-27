import type { BrowserWindow } from 'electron';
import type AppAda from '@cardano-foundation/ledgerjs-hw-app-cardano';
import type TransportNodeHid from '@ledgerhq/hw-transport-node-hid-noevents';
import TrezorConnect from '@trezor/connect';

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

jest.mock('@trezor/connect', () => ({
  __esModule: true,
  default: {
    removeAllListeners: jest.fn(),
    cancel: jest.fn(),
    on: jest.fn(),
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

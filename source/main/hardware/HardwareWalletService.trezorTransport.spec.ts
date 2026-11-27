import TrezorConnect from '@trezor/connect';
import log from 'electron-log-daedalus';
import type { HardwareWalletChannels } from '../ipc/createHardwareWalletIPCChannels';
import {
  HardwareWalletOperationCancelled,
  HardwareWalletService,
} from './HardwareWalletService';

jest.mock('@trezor/connect', () => ({
  __esModule: true,
  default: {
    removeAllListeners: jest.fn(),
    cancel: jest.fn(),
    on: jest.fn(),
  },
  DEVICE: { CONNECT: 'connect', DISCONNECT: 'disconnect', CHANGED: 'changed' },
  DEVICE_EVENT: 'device-event',
  TRANSPORT: { ERROR: 'transport-error', START: 'transport-start' },
  TRANSPORT_EVENT: 'transport-event',
  UI: { REQUEST_PASSPHRASE: 'request-passphrase' },
  UI_EVENT: 'ui-event',
}));
jest.mock('../trezor/connection', () => ({
  initTrezorConnect: jest.fn(() => Promise.resolve()),
  reinitTrezorConnect: jest.fn(() => Promise.resolve()),
}));
jest.mock('../ipc/electronStoreConversation', () => ({
  requestElectronStore: jest.fn(),
}));
jest.mock('electron-log-daedalus', () => ({
  __esModule: true,
  default: {
    debug: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
    error: jest.fn(),
  },
}));
jest.mock('@ledgerhq/hw-transport-node-hid-noevents', () => ({
  __esModule: true,
  default: { open: jest.fn(), list: jest.fn() },
  getDevices: jest.fn(),
}));
jest.mock('@cardano-foundation/ledgerjs-hw-app-cardano', () => ({
  __esModule: true,
  default: class {},
  AddressType: {},
  MessageAddressFieldType: {},
  utils: {},
}));
jest.mock('../config', () => ({
  dappRuntimeConfig: {
    cip30Enabled: true,
    dappSandboxPackageCluster: 'preprod',
  },
  dappLaunchPolicy: { hardwareConnectorEnabled: jest.fn(() => true) },
}));

type TransportListener = (event: { type: string; payload?: unknown }) => void;

const setup = async () => {
  const handlers = new Map<string, () => Promise<unknown>>();
  const send = jest.fn(() => Promise.resolve(undefined));
  const channels = new Proxy(
    {},
    {
      get: (_target, name: string) => ({
        onRequest: (handler) => handlers.set(name, handler),
        send,
      }),
    }
  ) as HardwareWalletChannels;
  const service = new HardwareWalletService();
  await service.register(channels);
  await handlers.get('handleInitTrezorConnectChannel')!();
  const listener = (TrezorConnect.on as jest.Mock).mock.calls.find(
    ([name]) => name === 'transport-event'
  )![1] as TransportListener;
  return { service, handlers, listener, send };
};

const transportError = (payload: Record<string, unknown>) => ({
  type: 'transport-error',
  payload,
});
const refused = { apiType: 'usb', error: 'Network request failed' };
const timedOut = { apiType: 'usb', error: 'Aborted by timeout' };
// What the real logger writes: the message text and the filtered data.
const written = (text: string) =>
  (log.info as jest.Mock).mock.calls
    .map(([, entry]) => entry as { message: string; data: unknown })
    .filter(({ message }) => message.includes(text));
const errorLines = () => written('Received TRANSPORT_EVENT: transport-error');
const summaryLines = () => written('transport-error repeated');

describe('HardwareWalletService Trezor transport errors', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    jest.spyOn(Date, 'now').mockReturnValue(1000);
  });
  afterEach(() => jest.restoreAllMocks());

  it('logs and forwards a run of identical errors once without cancelling', async () => {
    const { listener, send } = await setup();
    for (let i = 0; i < 5; i++) listener(transportError(refused));
    expect(errorLines()).toHaveLength(1);
    expect(errorLines()[0].message).toContain('Network request failed');
    expect(errorLines()[0].data).toEqual({ apiType: 'usb' });
    expect(send).toHaveBeenCalledTimes(1);
    expect(TrezorConnect.cancel).not.toHaveBeenCalled();
  });

  it('logs a changed error with a summary of the repeats before it', async () => {
    const { listener, send } = await setup();
    const now = jest.spyOn(Date, 'now');
    now.mockReturnValue(1000);
    listener(transportError(refused));
    now.mockReturnValue(4000);
    listener(transportError(refused));
    listener(transportError(refused));
    listener(transportError(timedOut));
    expect(errorLines()).toHaveLength(2);
    expect(errorLines()[1].message).toContain('Aborted by timeout');
    expect(summaryLines()).toHaveLength(1);
    expect(summaryLines()[0].message).toContain(
      'repeated 2 more times over 3s'
    );
    expect(summaryLines()[0].message).toContain('Network request failed');
    expect(send).toHaveBeenCalledTimes(2);
  });

  it('logs one summary with the count when the transport starts again', async () => {
    const { listener, send } = await setup();
    listener(transportError(refused));
    listener(transportError(refused));
    listener(transportError(refused));
    listener({ type: 'transport-start', payload: {} });
    listener({ type: 'transport-start', payload: {} });
    expect(summaryLines()).toHaveLength(1);
    expect(summaryLines()[0].message).toContain('repeated 2 more times');
    listener(transportError(refused));
    expect(errorLines()).toHaveLength(2);
    expect(send).toHaveBeenCalledTimes(2);
  });

  it('logs the summary when the listeners are reset and when it is disposed', async () => {
    const { service, handlers, listener } = await setup();
    listener(transportError(refused));
    listener(transportError(refused));
    await handlers.get('handleInitTrezorConnectChannel')!();
    expect(summaryLines()).toHaveLength(1);
    listener(transportError(timedOut));
    listener(transportError(timedOut));
    await service.dispose();
    expect(summaryLines()).toHaveLength(2);
    expect(summaryLines()[1].message).toContain('Aborted by timeout');
  });

  it('cancels the active operation once on an error other than a refused connection', async () => {
    const { service, listener } = await setup();
    let finish: () => void = () => undefined;
    const { withTrezorOperation } = service as unknown as {
      withTrezorOperation: (execute: () => Promise<void>) => Promise<void>;
    };
    const operation = withTrezorOperation(
      () =>
        new Promise<void>((resolve) => {
          finish = resolve;
        })
    );
    listener(transportError(timedOut));
    listener(transportError(timedOut));
    finish();
    await expect(operation).rejects.toBeInstanceOf(
      HardwareWalletOperationCancelled
    );
    expect(TrezorConnect.cancel).toHaveBeenCalledTimes(1);
    expect(TrezorConnect.cancel).toHaveBeenCalledWith('Method_Cancel');
  });

  it('does not cancel on a refused bridge connection', async () => {
    const { listener } = await setup();
    listener(transportError(refused));
    listener(transportError(refused));
    expect(TrezorConnect.cancel).not.toHaveBeenCalled();
  });
});

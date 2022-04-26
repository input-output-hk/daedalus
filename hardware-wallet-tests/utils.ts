import expect from 'expect';
import createIPCMock from 'electron-mock-ipc';
import chalk from 'chalk';
import {
  IpcChannel,
  IpcReceiver,
  IpcSender,
} from '../source/common/ipc/lib/IpcChannel';
import {
  GET_INIT_LEDGER_CONNECT_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  DEVICE_NOT_CONNECTED,
} from '../source/common/ipc/api';

import { createChannels } from '../source/main/ipc/createHardwareWalletIPCChannels';
import { handleHardwareWalletRequests } from '../source/main/ipc/getHardwareWalletChannel';

export const { ipcMain, ipcRenderer } = createIPCMock();

export class MockIpcChannel<Incoming, Outgoing> extends IpcChannel<
  Incoming,
  Outgoing
> {
  async send(
    message: Outgoing,
    sender: IpcSender = ipcRenderer,
    receiver: IpcReceiver = ipcRenderer
  ): Promise<Incoming> {
    return super.send(message, sender, receiver);
  }

  async request(
    message: Outgoing,
    sender: IpcSender = ipcMain,
    receiver: IpcReceiver = ipcMain
  ): Promise<Incoming> {
    return super.request(message, sender, receiver);
  }

  onReceive(
    handler: (message: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onReceive(handler, receiver);
  }

  onRequest(
    handler: (arg0: Incoming) => Promise<Outgoing>,
    receiver: IpcReceiver = ipcMain
  ): void {
    super.onRequest(handler, receiver);
  }
}

export const createAndRegisterHardwareWalletChannels = () =>
  // @ts-ignore Argument of type 'ipcRenderer' is not assignable to parameter of type 'BrowserWindow'.
  handleHardwareWalletRequests(ipcRenderer, createChannels(MockIpcChannel));

export const initLedgerChannel = () => {
  const initLedgerConnectChannel = new MockIpcChannel(
    GET_INIT_LEDGER_CONNECT_CHANNEL
  );
  initLedgerConnectChannel.request({}, ipcRenderer, ipcMain);
};

export const createCardanoAppChannel = () =>
  new MockIpcChannel(GET_CARDANO_ADA_APP_CHANNEL);

export const createGetPublicKeyChannel = () =>
  new MockIpcChannel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);

export const createHardwareWalletConnectionChannel = () =>
  new MockIpcChannel(GET_HARDWARE_WALLET_CONNECTION_CHANNEL);

export const requestLaunchingCardanoAppOnLedger = (deviceId: string) =>
  new Promise((resolve, reject) => {
    const cardanoAppChannel = createCardanoAppChannel();

    const run = async () => {
      try {
        const cardanoAppChannelResponse = await cardanoAppChannel.request(
          { path: deviceId },
          ipcRenderer,
          ipcRenderer
        );
        return resolve(cardanoAppChannelResponse);
      } catch (err) {
        if (err.code === DEVICE_NOT_CONNECTED) {
          return reject(err);
        }

        setTimeout(run, 1000);
      }
    };

    run();
  });

interface Result {
  disconnected: boolean;
  deviceType: string;
  deviceId: string | null;
  deviceModel: string;
  deviceName: string;
  path: string;
}

export const createSequentialResult = (sequence: Array<Partial<Result>>) => {
  const common = {
    disconnected: expect.any(Boolean),
    deviceType: expect.any(String),
    deviceId: null,
    deviceModel: expect.any(String),
    deviceName: expect.any(String),
    path: expect.any(String),
    product: expect.any(String),
  };

  const result = sequence.map((s) => ({ ...common, ...s }));

  return () => [result.shift(), result.length === 0];
};

export const log = (message: string) =>
  console.log(chalk.whiteBright.bgBlackBright.bold(message)); // eslint-disable-line no-console

export const createTestInstructions = (messages: string[]) => {
  messages.forEach((m, i) => log(`${i + 1} - ${m}`));
};

export const waitForZombieMessages = () =>
  new Promise((resolve) => setTimeout(resolve, 1000));

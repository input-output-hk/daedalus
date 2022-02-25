import expect from 'expect';
import chalk from 'chalk';
import createIPCMock from 'electron-mock-ipc';
import {
  IpcChannel,
  IpcReceiver,
  IpcSender,
} from '../source/common/ipc/lib/IpcChannel';

import { createChannels } from '../source/main/ipc/createHardwareWalletIPCChannels';
import { handleHardwareWalletRequests } from '../source/main/ipc/getHardwareWalletChannel';

const mocked = createIPCMock();
const { ipcMain } = mocked;
const { ipcRenderer } = mocked;

export { ipcMain, ipcRenderer };

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
  // @ts-ignore fix-me later
  handleHardwareWalletRequests(ipcRenderer, createChannels(MockIpcChannel));

export const initLedgerChannel = () => {
  const initLedgerConnectChannel = new MockIpcChannel(
    'GET_INIT_LEDGER_CONNECT_CHANNEL'
  );
  initLedgerConnectChannel.request({}, ipcRenderer, ipcMain);
};

export const createCardanoAppChannel = () =>
  new MockIpcChannel('GET_CARDANO_ADA_APP_CHANNEL');

export const createGetPublicKeyChannel = () =>
  new MockIpcChannel('GET_EXTENDED_PUBLIC_KEY_CHANNEL');

export const createHardwareWalletConnectionChannel = () =>
  new MockIpcChannel('GET_HARDWARE_WALLET_CONNECTION_CHANNEL');

export const pollCardarnoApp = (deviceId: string) =>
  new Promise((resolve, reject) => {
    const cardanoAppChannel = createCardanoAppChannel();

    const interval = setInterval(async () => {
      try {
        const cardanoAppChannelReply = await cardanoAppChannel.request(
          { path: deviceId },
          ipcRenderer,
          ipcRenderer
        );
        clearInterval(interval);
        return resolve(cardanoAppChannelReply);
      } catch (err) {
        if (err.code === 'DEVICE_NOT_CONNECTED') {
          clearInterval(interval);
          return reject(err);
        }
        return null;
      }
    }, 2000);
  });

export const createSequentialResult = (sequence) => {
  const common = {
    disconnected: expect.any(Boolean),
    deviceType: expect.any(String),
    deviceId: null,
    deviceModel: expect.any(String),
    deviceName: expect.any(String),
    path: expect.any(String),
  };

  const result = sequence.map((s) => ({ ...common, ...s }));

  return () => [result.shift(), result.length === 0];
};

export const log = (message: string) =>
  console.log(chalk.whiteBright.bgBlackBright.bold(message)); // eslint-disable-line no-console

export const createSequentialPromptMessages = (messages: string[]) => {
  messages.forEach((m, i) => log(`${i + 1} - ${m}`));

  return () => log(messages.shift());
};

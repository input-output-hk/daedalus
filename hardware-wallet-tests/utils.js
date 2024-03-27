'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
var _a;
Object.defineProperty(exports, '__esModule', { value: true });
exports.waitForZombieMessages = exports.createTestInstructions = exports.log = exports.createSequentialResult = exports.requestLaunchingCardanoAppOnLedger = exports.createHardwareWalletConnectionChannel = exports.createGetPublicKeyChannel = exports.createCardanoAppChannel = exports.initLedgerChannel = exports.createAndRegisterHardwareWalletChannels = exports.MockIpcChannel = exports.ipcRenderer = exports.ipcMain = void 0;
const expect_1 = __importDefault(require('expect'));
const electron_mock_ipc_1 = __importDefault(require('electron-mock-ipc'));
const chalk_1 = __importDefault(require('chalk'));
const IpcChannel_1 = require('../source/common/ipc/lib/IpcChannel');
const api_1 = require('../source/common/ipc/api');
const createHardwareWalletIPCChannels_1 = require('../source/main/ipc/createHardwareWalletIPCChannels');
const getHardwareWalletChannel_1 = require('../source/main/ipc/getHardwareWalletChannel');
(_a = (0, electron_mock_ipc_1.default)()),
  (exports.ipcMain = _a.ipcMain),
  (exports.ipcRenderer = _a.ipcRenderer);
class MockIpcChannel extends IpcChannel_1.IpcChannel {
  async send(
    message,
    sender = exports.ipcRenderer,
    receiver = exports.ipcRenderer
  ) {
    return super.send(message, sender, receiver);
  }
  async request(message, sender = exports.ipcMain, receiver = exports.ipcMain) {
    return super.request(message, sender, receiver);
  }
  onReceive(handler, receiver = exports.ipcMain) {
    super.onReceive(handler, receiver);
  }
  onRequest(handler, receiver = exports.ipcMain) {
    super.onRequest(handler, receiver);
  }
}
exports.MockIpcChannel = MockIpcChannel;
const createAndRegisterHardwareWalletChannels = () =>
  // @ts-ignore Argument of type 'ipcRenderer' is not assignable to parameter of type 'BrowserWindow'.
  (0, getHardwareWalletChannel_1.handleHardwareWalletRequests)(
    exports.ipcRenderer,
    (0, createHardwareWalletIPCChannels_1.createChannels)(MockIpcChannel)
  );
exports.createAndRegisterHardwareWalletChannels = createAndRegisterHardwareWalletChannels;
const initLedgerChannel = () => {
  const initLedgerConnectChannel = new MockIpcChannel(
    api_1.GET_INIT_LEDGER_CONNECT_CHANNEL
  );
  initLedgerConnectChannel.request({}, exports.ipcRenderer, exports.ipcMain);
};
exports.initLedgerChannel = initLedgerChannel;
const createCardanoAppChannel = () =>
  new MockIpcChannel(api_1.GET_CARDANO_ADA_APP_CHANNEL);
exports.createCardanoAppChannel = createCardanoAppChannel;
const createGetPublicKeyChannel = () =>
  new MockIpcChannel(api_1.GET_EXTENDED_PUBLIC_KEY_CHANNEL);
exports.createGetPublicKeyChannel = createGetPublicKeyChannel;
const createHardwareWalletConnectionChannel = () =>
  new MockIpcChannel(api_1.GET_HARDWARE_WALLET_CONNECTION_CHANNEL);
exports.createHardwareWalletConnectionChannel = createHardwareWalletConnectionChannel;
const requestLaunchingCardanoAppOnLedger = (deviceId) =>
  new Promise((resolve, reject) => {
    const cardanoAppChannel = (0, exports.createCardanoAppChannel)();
    const run = async () => {
      try {
        const cardanoAppChannelResponse = await cardanoAppChannel.request(
          { path: deviceId },
          exports.ipcRenderer,
          exports.ipcRenderer
        );
        return resolve(cardanoAppChannelResponse);
      } catch (err) {
        if (err.code === api_1.DEVICE_NOT_CONNECTED) {
          return reject(err);
        }
        setTimeout(run, 1000);
      }
    };
    run();
  });
exports.requestLaunchingCardanoAppOnLedger = requestLaunchingCardanoAppOnLedger;
const createSequentialResult = (sequence) => {
  const common = {
    disconnected: expect_1.default.any(Boolean),
    deviceType: expect_1.default.any(String),
    deviceId: null,
    deviceModel: expect_1.default.any(String),
    deviceName: expect_1.default.any(String),
    path: expect_1.default.any(String),
    product: expect_1.default.any(String),
  };
  const result = sequence.map((s) => ({ ...common, ...s }));
  return () => [result.shift(), result.length === 0];
};
exports.createSequentialResult = createSequentialResult;
const log = (message) =>
  console.log(chalk_1.default.whiteBright.bgBlackBright.bold(message)); // eslint-disable-line no-console
exports.log = log;
const createTestInstructions = (messages) => {
  messages.forEach((m, i) => (0, exports.log)(`${i + 1} - ${m}`));
};
exports.createTestInstructions = createTestInstructions;
const waitForZombieMessages = () =>
  new Promise((resolve) => setTimeout(resolve, 1000));
exports.waitForZombieMessages = waitForZombieMessages;
//# sourceMappingURL=utils.js.map

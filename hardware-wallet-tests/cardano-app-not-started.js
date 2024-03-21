'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.run = void 0;
const expect_1 = __importDefault(require('expect'));
const utils_1 = require('./utils');
const run = () => {
  expect_1.default.assertions(3);
  (0, utils_1.createTestInstructions)([
    'Start test runner',
    'Plug Ledger Nano S to your computer',
    'Start Cardano APP on Nano S',
    'Nano S will prompt to export the public key',
    'Export the public key',
  ]);
  (0, utils_1.createAndRegisterHardwareWalletChannels)();
  const publicKeyChannel = (0, utils_1.createGetPublicKeyChannel)();
  const hardwareWalletConnectionChannel = (0,
  utils_1.createHardwareWalletConnectionChannel)();
  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(async (params) => {
      (0, expect_1.default)(params).toEqual({
        disconnected: expect_1.default.any(Boolean),
        deviceType: expect_1.default.any(String),
        deviceId: null,
        deviceModel: expect_1.default.any(String),
        deviceName: expect_1.default.any(String),
        path: expect_1.default.any(String),
        product: expect_1.default.any(String),
      });
      const cardanoAppChannelResponse = await (0,
      utils_1.requestLaunchingCardanoAppOnLedger)(params.path);
      (0, expect_1.default)(cardanoAppChannelResponse).toEqual({
        minor: expect_1.default.any(Number),
        major: expect_1.default.any(Number),
        patch: expect_1.default.any(Number),
        deviceId: expect_1.default.any(String),
      });
      const extendedPublicKey = await publicKeyChannel.request(
        {
          path: "1852'/1815'/0'",
          // Shelley 1852 ADA 1815 indicator for account '0'
          isTrezor: false,
          devicePath: params.path,
        },
        utils_1.ipcRenderer,
        utils_1.ipcRenderer
      );
      (0, expect_1.default)(extendedPublicKey).toEqual({
        chainCodeHex: expect_1.default.any(String),
        publicKeyHex: expect_1.default.any(String),
        deviceId: expect_1.default.any(String),
      });
      resolve();
    });
    (0, utils_1.initLedgerChannel)();
  });
};
exports.run = run;
//# sourceMappingURL=cardano-app-not-started.js.map

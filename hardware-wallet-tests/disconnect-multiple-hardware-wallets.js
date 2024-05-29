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
const getNextExpectedSequence = (0, utils_1.createSequentialResult)([
  {
    disconnected: false,
    deviceModel: 'nanoS',
  },
  {
    disconnected: false,
    deviceModel: 'nanoSP',
  },
  {
    disconnected: false,
    deviceModel: 'nanoX',
  },
  {
    disconnected: true,
    deviceModel: 'nanoS',
  },
  {
    disconnected: true,
    deviceModel: 'nanoSP',
  },
  {
    disconnected: true,
    deviceModel: 'nanoX',
  },
]);
const run = () => {
  expect_1.default.assertions(4);
  (0, utils_1.createAndRegisterHardwareWalletChannels)();
  const hardwareWalletConnectionChannel = (0,
  utils_1.createHardwareWalletConnectionChannel)();
  (0, utils_1.createTestInstructions)([
    'Connect Nano S',
    'Connect Nano S Plus',
    'Connect Nano X',
    'Disconnect Nano S',
    'Disconnect Nano S Plus',
    'Disconnect Nano X',
  ]);
  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(async (params) => {
      const [expectedValue, isOver] = getNextExpectedSequence();
      (0, expect_1.default)(params).toEqual(expectedValue);
      if (isOver) {
        await (0, utils_1.waitForZombieMessages)();
        return resolve();
      }
    });
    (0, utils_1.initLedgerChannel)();
  });
};
exports.run = run;
//# sourceMappingURL=disconnect-multiple-hardware-wallets.js.map

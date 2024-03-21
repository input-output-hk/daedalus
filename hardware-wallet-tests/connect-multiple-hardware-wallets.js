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
  expect_1.default.assertions(2);
  (0, utils_1.createAndRegisterHardwareWalletChannels)();
  const hardwareWalletConnectionChannel = (0,
  utils_1.createHardwareWalletConnectionChannel)();
  (0, utils_1.createTestInstructions)([
    'Start test runner',
    'Plug Ledger Nano S to your computer',
    'Plug Ledger Nano S Plus to your computer',
    'Plug Ledger Nano X to your computer',
  ]);
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
  ]);
  return new Promise((resolve) => {
    hardwareWalletConnectionChannel.onReceive(async (message) => {
      const [expectedValue, isOver] = getNextExpectedSequence();
      (0, expect_1.default)(message).toEqual(expectedValue);
      if (isOver) {
        await (0, utils_1.waitForZombieMessages)();
        resolve();
      }
    });
    (0, utils_1.initLedgerChannel)();
  });
};
exports.run = run;
//# sourceMappingURL=connect-multiple-hardware-wallets.js.map

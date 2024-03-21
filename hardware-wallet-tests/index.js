'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const prompts_1 = __importDefault(require('prompts'));
const disconnect_multiple_hardware_wallets_1 = require('./disconnect-multiple-hardware-wallets');
const cardano_app_already_launched_1 = require('./cardano-app-already-launched');
const cardano_app_not_started_1 = require('./cardano-app-not-started');
const disconnect_single_hardware_wallet_1 = require('./disconnect-single-hardware-wallet');
const connect_multiple_hardware_wallets_1 = require('./connect-multiple-hardware-wallets');
const CARDANO_APP_ALREADY_LAUNCHED = 'CARDANO_APP_ALREADY_LAUNCHED';
const CARDANO_APP_NOT_STARTED = 'CARDANO_APP_NOT_STARTED';
const SINGLE_LEDGER_DISCONNECTED = 'SINGLE_LEDGER_DISCONNECTED';
const MULTIPLE_HARDWARE_WALLETS = 'MULTIPLE_HARDWARE_WALLETS';
const MULTIPLE_HARDWARE_WALLETS_REMOVED = 'MULTIPLE_HARDWARE_WALLETS_REMOVED';
(async () => {
  const { testType } = await (0, prompts_1.default)({
    type: 'select',
    name: 'testType',
    message: 'Ledger Hardware Wallets Channel',
    choices: [
      {
        title: 'export public key when Cardano APP is already launched',
        value: CARDANO_APP_ALREADY_LAUNCHED,
      },
      {
        title:
          'export public key when Cardano APP is launched after running HW test script',
        value: CARDANO_APP_NOT_STARTED,
      },
      {
        title: 'detect when ledger is disconnected from computer',
        value: SINGLE_LEDGER_DISCONNECTED,
      },
      {
        title: 'connect both Nano S and Nano X',
        value: MULTIPLE_HARDWARE_WALLETS,
      },
      {
        title: 'detect when multiple hardware wallets are removed',
        value: MULTIPLE_HARDWARE_WALLETS_REMOVED,
      },
      { title: 'exit', value: 'exit' },
    ],
  });
  switch (testType) {
    case CARDANO_APP_ALREADY_LAUNCHED:
      await (0, cardano_app_already_launched_1.run)();
      break;
    case CARDANO_APP_NOT_STARTED:
      await (0, cardano_app_not_started_1.run)();
      break;
    case SINGLE_LEDGER_DISCONNECTED:
      await (0, disconnect_single_hardware_wallet_1.run)();
      break;
    case MULTIPLE_HARDWARE_WALLETS:
      await (0, connect_multiple_hardware_wallets_1.run)();
      break;
    case MULTIPLE_HARDWARE_WALLETS_REMOVED:
      await (0, disconnect_multiple_hardware_wallets_1.run)();
      break;
    default:
      break;
  }
  process.exit(0);
})();
//# sourceMappingURL=index.js.map

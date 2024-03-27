'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.BlockSyncType = exports.NetworkMagics = exports.FaultInjections = exports.CardanoProcessNameOptions = exports.CardanoNodeStates = exports.NetworkNameOptions = exports.CardanoNodeImplementationOptions = void 0;
const environment_types_1 = require('./environment.types');
exports.CardanoNodeImplementationOptions = {
  CARDANO: 'cardano',
  SELFNODE: 'selfnode',
};
exports.NetworkNameOptions = {
  mainnet: 'mainnet',
  testnet: 'testnet',
  staging: 'staging',
  shelley_qa: 'shelley_qa',
  alonzo_purple: 'alonzo_purple',
  vasil_dev: 'vasil_dev',
  preprod: 'preprod',
  preview: 'preview',
  selfnode: 'selfnode',
  development: 'development',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.CardanoNodeStates = {
  STARTING: 'starting',
  RUNNING: 'running',
  EXITING: 'exiting',
  STOPPING: 'stopping',
  STOPPED: 'stopped',
  UPDATING: 'updating',
  UPDATED: 'updated',
  CRASHED: 'crashed',
  ERRORED: 'errored',
  UNRECOVERABLE: 'unrecoverable',
};
exports.CardanoProcessNameOptions = {
  cardano: {
    win32: 'cardano-node.exe',
    linux: 'cardano-node',
    darwin: 'cardano-node',
  },
  selfnode: {
    win32: 'local-cluster.exe',
    linux: 'local-cluster',
    darwin: 'local-cluster',
  },
};
exports.FaultInjections = {
  IgnoreShutdown: 'FInjIgnoreShutdown',
  IgnoreApi: 'FInjIgnoreAPI',
  ApplyUpdateNoExit: 'FInjApplyUpdateNoExit',
  ApplyUpdateWrongExitCode: 'FInjApplyUpdateWrongExitCode',
};
exports.NetworkMagics = {
  // Cardano Mainet network magic
  [environment_types_1.MAINNET]: [1, null],
  // Cardano Staging network magic
  [environment_types_1.STAGING]: [633343913, 1],
  // Cardano Byron Testnet network magic
  [environment_types_1.TESTNET]: [1097911063, 0],
  // Cardano Alonzo Purple network magic
  [environment_types_1.ALONZO_PURPLE]: [8, 0],
  // Cardano Vasil-Dev network magic
  [environment_types_1.VASIL_DEV]: [9, 0],
  // Cardano Pre-Prod network magic
  [environment_types_1.PREPROD]: [1, 0],
  // Cardano Preview network magic
  [environment_types_1.PREVIEW]: [2, 0],
  // Cardano Shelley QA network magic
  [environment_types_1.SHELLEY_QA]: [3, 0],
  // Cardano Selfnode network magic
  [environment_types_1.SELFNODE]: [1, null],
};
var BlockSyncType;
(function (BlockSyncType) {
  BlockSyncType['pushingLedger'] = 'pushingLedger';
  BlockSyncType['replayedBlock'] = 'replayedBlock';
  BlockSyncType['validatingChunk'] = 'validatingChunk';
})((BlockSyncType = exports.BlockSyncType || (exports.BlockSyncType = {})));
//# sourceMappingURL=cardano-node.types.js.map

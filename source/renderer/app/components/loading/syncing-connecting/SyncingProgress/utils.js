'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getProgressNameByBlockSyncType = exports.getProgressDescriptionByBlockSyncType = void 0;
const cardano_node_types_1 = require('../../../../../../common/types/cardano-node.types');
const SyncingProgress_messages_1 = __importDefault(
  require('./SyncingProgress.messages')
);
const mapOfBlockSyncTypeToProgressDescription = {
  [cardano_node_types_1.BlockSyncType.validatingChunk]:
    SyncingProgress_messages_1.default.validatingChunkDescription,
  [cardano_node_types_1.BlockSyncType.replayedBlock]:
    SyncingProgress_messages_1.default.replayedBlockDescription,
  [cardano_node_types_1.BlockSyncType.pushingLedger]:
    SyncingProgress_messages_1.default.pushingLedgerDescription,
};
const getProgressDescriptionByBlockSyncType = (type) =>
  mapOfBlockSyncTypeToProgressDescription[type];
exports.getProgressDescriptionByBlockSyncType = getProgressDescriptionByBlockSyncType;
const mapOfBlockSyncTypeToProgressName = {
  [cardano_node_types_1.BlockSyncType.validatingChunk]:
    SyncingProgress_messages_1.default.validatingChunk,
  [cardano_node_types_1.BlockSyncType.replayedBlock]:
    SyncingProgress_messages_1.default.replayedBlock,
  [cardano_node_types_1.BlockSyncType.pushingLedger]:
    SyncingProgress_messages_1.default.pushingLedger,
};
const getProgressNameByBlockSyncType = (type) =>
  mapOfBlockSyncTypeToProgressName[type];
exports.getProgressNameByBlockSyncType = getProgressNameByBlockSyncType;
//# sourceMappingURL=utils.js.map

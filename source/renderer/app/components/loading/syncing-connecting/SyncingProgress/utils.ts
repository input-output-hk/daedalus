import { BlockSyncType } from '../../../../../../common/types/cardano-node.types';
import { ReactIntlMessage } from '../../../../types/i18nTypes';
import messages from './SyncingProgress.messages';

const mapOfBlockSyncTypeToProgressDescription: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.replayedBlock]: messages.verifyingBlockchainDescription,
  [BlockSyncType.validatingChunk]: messages.validatingChunkDescription,
  [BlockSyncType.pushingLedger]: messages.pushingLedgerStateDescription,
};

const getProgressDescriptionByBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToProgressDescription[type];

const mapOfBlockSyncTypeToProgressName: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.replayedBlock]: messages.verifyingBlockchain,
  [BlockSyncType.validatingChunk]: messages.validatingChunk,
  [BlockSyncType.pushingLedger]: messages.pushingLedgerState,
};

const getProgressNameByBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToProgressName[type];

export {
  getProgressDescriptionByBlockSyncType,
  getProgressNameByBlockSyncType,
};

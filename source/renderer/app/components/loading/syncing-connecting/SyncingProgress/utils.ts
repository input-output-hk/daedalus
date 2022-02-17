import { BlockSyncType } from '../../../../../../common/types/cardano-node.types';
import { ReactIntlMessage } from '../../../../types/i18nTypes';
import messages from './SyncingProgress.messages';

const mapOfBlockSyncTypeToProgressDescription: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.validatingChunk]: messages.validatingChunkDescription,
  [BlockSyncType.replayedBlock]: messages.replayedBlockDescription,
  [BlockSyncType.pushingLedger]: messages.pushingLedgerDescription,
};

const getProgressDescriptionByBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToProgressDescription[type];

const mapOfBlockSyncTypeToProgressName: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.validatingChunk]: messages.validatingChunk,
  [BlockSyncType.replayedBlock]: messages.replayedBlock,
  [BlockSyncType.pushingLedger]: messages.pushingLedger,
};

const getProgressNameByBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToProgressName[type];

export {
  getProgressDescriptionByBlockSyncType,
  getProgressNameByBlockSyncType,
};

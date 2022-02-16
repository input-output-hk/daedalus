import { BlockSyncType } from '../../../../../../common/types/cardano-node.types';
import { ReactIntlMessage } from '../../../../types/i18nTypes';
import messages from './SyncingProgress.messages';

const mapOfBlockSyncTypeToMessageDescription: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.replayedBlock]: messages.verifyingBlockchainDescription,
  [BlockSyncType.validatingChunk]: messages.validatingChunkDescription,
  [BlockSyncType.pushingLedger]: messages.pushingLedgerStateDescription,
};

const getDescriptionOfBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToMessageDescription[type];

const mapOfBlockSyncTypeToMessage: {
  [key in BlockSyncType]: ReactIntlMessage;
} = {
  [BlockSyncType.replayedBlock]: messages.verifyingBlockchain,
  [BlockSyncType.validatingChunk]: messages.validatingChunk,
  [BlockSyncType.pushingLedger]: messages.pushingLedgerState,
};

const getMessageOfBlockSyncType = (type: BlockSyncType) =>
  mapOfBlockSyncTypeToMessage[type];

export { getDescriptionOfBlockSyncType, getMessageOfBlockSyncType };

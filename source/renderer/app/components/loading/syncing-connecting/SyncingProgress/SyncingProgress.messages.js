'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const react_intl_1 = require('react-intl');
const messages = (0, react_intl_1.defineMessages)({
  validatingChunk: {
    id: 'loading.screen.validatingChunk',
    defaultMessage: '!!!Verifying on-disk blockchain state',
    description: 'One of three progress names on the loading screen.',
  },
  validatingChunkDescription: {
    id: 'loading.screen.validatingChunkDescription',
    defaultMessage:
      '!!!Verifying the integrity of the blockchain calculating hashes',
    description:
      'Description of one of three progress names on the loading screen.',
  },
  replayedBlock: {
    id: 'loading.screen.replayedBlock',
    defaultMessage: '!!!Replaying ledger from on-disk blockchain',
    description: 'One of three progress names on the loading screen.',
  },
  replayedBlockDescription: {
    id: 'loading.screen.replayedBlockDescription',
    defaultMessage:
      '!!!Looking for a ledger snapshot and updating (recomputing) the latest state',
    description:
      'Description of one of three progress names on the loading screen.',
  },
  pushingLedger: {
    id: 'loading.screen.pushingLedger',
    defaultMessage: '!!!Syncing blockchain',
    description: 'One of three progress names on the loading screen.',
  },
  pushingLedgerDescription: {
    id: 'loading.screen.pushingLedgerDescription',
    defaultMessage:
      '!!!Performing initial chain selection and finalizing blockchain state',
    description:
      'Description of one of three progress names on the loading screen.',
  },
});
exports.default = messages;
//# sourceMappingURL=SyncingProgress.messages.js.map

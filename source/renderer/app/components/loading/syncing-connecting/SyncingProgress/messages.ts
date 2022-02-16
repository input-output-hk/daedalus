import { defineMessages } from 'react-intl';
import { ReactIntlMessage } from '../../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  verifyingBlockchain: {
    id: 'loading.screen.verifyingBlockchainMessage',
    defaultMessage: '!!!Verifying the blockchain',
    description: 'Message "Verifying the blockchain" on the loading screen.',
  },
  verifyingBlockchainDescription: {
    id: 'loading.screen.verifyingBlockchainDescription',
    defaultMessage:
      '!!!This process replays the blockchain from the last saved ledger',
    description:
      'Description of "Verifying the blockchain" message on the loading screen.',
  },
  validatingChunk: {
    id: 'loading.screen.validatingChunk',
    defaultMessage: '!!!Validating blocks',
    description: 'Message "Validating blocks" on the loading screen.',
  },
  validatingChunkDescription: {
    id: 'loading.screen.validatingChunkDescription',
    defaultMessage:
      '!!!This process verifies the integrity of locally stored blockchain',
    description:
      'Description of "Validating blocks" message on the loading screen.',
  },
  pushingLedgerState: {
    id: 'loading.screen.pushingLedgerState',
    defaultMessage: '!!!Applying block',
    description: 'Message "Applying a block to ledger" on the loading screen.',
  },
  pushingLedgerStateDescription: {
    id: 'loading.screen.pushingLedgerStateDescription',
    defaultMessage: '!!!This process applies a block to the ledger',
    description:
      'Description of "Applying block" message on the loading screen.',
  },
});

export default messages;

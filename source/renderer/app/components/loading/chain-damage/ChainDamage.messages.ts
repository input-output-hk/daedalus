import { defineMessages } from 'react-intl';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

const messages: Record<string, ReactIntlMessage> = defineMessages({
  title: {
    id: 'loading.chainDamage.title',
    defaultMessage: '!!!Blockchain data needs to be rebuilt',
    description:
      'Headline of the screen that offers to rebuild the blockchain data after Daedalus detects a gap in it',
  },
  symptomNodeRejectsCanonicalBlock: {
    id: 'loading.chainDamage.symptom.nodeRejectsCanonicalBlock',
    defaultMessage:
      '!!!Daedalus cannot finish syncing because part of the blockchain data stored on this computer is missing.',
    description:
      'Explanation shown when peers keep serving blocks that the local node rejects because blockchain data on this computer is missing',
  },
  symptomLedgerPointMissing: {
    id: 'loading.chainDamage.symptom.ledgerPointMissing',
    defaultMessage:
      '!!!Cardano node cannot start because part of the blockchain data stored on this computer is missing.',
    description:
      'Explanation shown when the node cannot open its database because blockchain data on this computer is missing',
  },
  symptomWalletChainDiscontinuity: {
    id: 'loading.chainDamage.symptom.walletChainDiscontinuity',
    defaultMessage:
      '!!!A wallet cannot finish syncing because part of the blockchain data stored on this computer is missing.',
    description:
      'Explanation shown when a wallet stops syncing because blockchain data on this computer is missing',
  },
  body: {
    id: 'loading.chainDamage.body',
    defaultMessage:
      '!!!Your wallets and funds are safe, and you do not need your recovery phrase. Daedalus can rebuild the blockchain data by downloading a verified copy with Mithril.',
    description:
      'Reassurance and description of the rebuild on the screen that offers to rebuild the blockchain data',
  },
  detail: {
    id: 'loading.chainDamage.detail',
    defaultMessage: '!!!Detected: {kind} at slot {slot}',
    description:
      'Technical detail line naming the kind of damage and the slot, for quoting in a support request',
  },
  detailNoSlot: {
    id: 'loading.chainDamage.detailNoSlot',
    defaultMessage: '!!!Detected: {kind}',
    description:
      'Technical detail line naming the kind of damage when no slot is known, for quoting in a support request',
  },
  rebuild: {
    id: 'loading.chainDamage.rebuild',
    defaultMessage: '!!!Rebuild blockchain data',
    description:
      'Button label that opens the confirmation for rebuilding the blockchain data',
  },
  notNow: {
    id: 'loading.chainDamage.notNow',
    defaultMessage: '!!!Not now',
    description:
      'Button label that hides the rebuild offer until Daedalus restarts',
  },
  confirmTitle: {
    id: 'loading.chainDamage.confirm.title',
    defaultMessage: '!!!Rebuild blockchain data?',
    description: 'Title of the confirmation for rebuilding the blockchain data',
  },
  confirmBody: {
    id: 'loading.chainDamage.confirm.body',
    defaultMessage:
      '!!!Daedalus will delete the blockchain data on this computer and download a complete verified copy. This is a large download, and Daedalus cannot be used until it finishes. Your wallets stay on this computer and finish syncing afterward.',
    description:
      'Body of the confirmation for rebuilding the blockchain data: what is deleted, what is downloaded, and that Daedalus is unusable meanwhile',
  },
  confirmRebuild: {
    id: 'loading.chainDamage.confirm.rebuild',
    defaultMessage: '!!!Rebuild',
    description:
      'Button label that starts rebuilding the blockchain data, in the confirmation',
  },
  confirmBack: {
    id: 'loading.chainDamage.confirm.back',
    defaultMessage: '!!!Back',
    description:
      'Button label that leaves the rebuild confirmation without rebuilding',
  },
});

export default messages;

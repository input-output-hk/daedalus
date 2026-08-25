import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  heading: {
    id: 'voting.governance.heading',
    defaultMessage: '!!!CARDANO VOTING POWER DELEGATION',
    description: 'Headline for Governance',
  },
  selectWalletLabel: {
    id: 'voting.governance.selectWalletLabel',
    defaultMessage: '!!!Delegate From',
    description: 'Label for the wallet select on the governance page',
  },
  selectWalletPlaceholder: {
    id: 'voting.governance.selectWalletPlaceholder',
    defaultMessage: '!!!Select a wallet…',
    description: 'Placeholder for the wallet select on the governance page',
  },
  initializeTxErrorGeneric: {
    id: 'voting.governance.transactionError.generic',
    defaultMessage:
      '!!!Something went wrong with this transaction. Please try again in a few minutes.',
    description:
      'Shown when a delegation transaction fails to build or to submit',
  },
  initializeTxErrorSameVote: {
    id: 'voting.governance.initializeTxError.sameVote',
    defaultMessage:
      '!!!This wallet already delegates to this choice. Please change delegation in order to proceed.',
    description:
      'Shown when the wallet is already delegated to the chosen option',
  },
  initializeNotEnoughMoney: {
    id: 'voting.governance.initializeTxError.notEnoughMoney',
    defaultMessage:
      '!!!This wallet does not contain the minimum required amount of ADA. The wallet might still be syncing. Please try again later or choose another wallet.',
    description:
      'Shown for both not_enough_money and no_utxos_available: to a reader they are one condition, too little in the wallet to build the transaction, and the wording covers a wallet that is merely still syncing',
  },
  browseDRepsButton: {
    id: 'voting.governance.browseDRepsButton',
    defaultMessage: '!!!Browse DReps',
    description: 'Button that opens the DRep directory to pick a DRep',
  },
});

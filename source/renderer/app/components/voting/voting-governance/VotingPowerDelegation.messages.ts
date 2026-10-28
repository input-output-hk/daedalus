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
  transactionErrorSameVote: {
    id: 'voting.governance.transactionError.sameVote',
    defaultMessage:
      '!!!This wallet already records a delegation of its voting power to this choice, so it will not send the same delegation again. If that delegation has ended, for example because the DRep retired, choose another DRep, Abstain or No Confidence.',
    description:
      'Shown when the chosen DRep, Abstain or No Confidence is the delegation the wallet already records, before submission or when the wallet refuses it as the same vote. The second sentence is the way out when that record outlived the delegation',
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
  choosePrompt: {
    id: 'voting.governance.chooseDelegation.prompt',
    defaultMessage:
      '!!!Choose Abstain, No Confidence or a DRep. Find a DRep in the Directory, or paste its ID.',
    description: 'Shown on the delegation form before anything is chosen',
  },
  pasteDRepIdPlaceholder: {
    id: 'voting.governance.pasteDRepId.placeholder',
    defaultMessage: '!!!Paste a DRep ID',
    description: 'Label and placeholder of the DRep ID field',
  },
  pasteDRepIdInvalid: {
    id: 'voting.governance.pasteDRepId.invalid',
    defaultMessage: '!!!This is not a valid DRep ID.',
    description: 'Shown under the field when the pasted value is not a DRep ID',
  },
  pasteDRepIdUse: {
    id: 'voting.governance.pasteDRepId.use',
    defaultMessage: '!!!Use this DRep ID',
    description: 'Button that makes the pasted ID the choice',
  },
});

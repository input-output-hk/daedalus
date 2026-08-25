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
    id: 'voting.governance.initializeTxError.generic',
    defaultMessage: '!!!Could not initialize transaction. Please try again!',
    description: 'Generic error for initialize transaction',
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
    description: 'Governance voting error when wallet has not enough funds',
  },
  browseDRepsButton: {
    id: 'voting.governance.browseDRepsButton',
    defaultMessage: '!!!Browse DReps',
    description: 'Button that opens the DRep directory to pick a DRep',
  },
  selectedDRepHeading: {
    id: 'voting.governance.selectedDRepHeading',
    defaultMessage: '!!!Delegate To',
    description:
      'Heading above the DRep card the user has selected to delegate to',
  },
});

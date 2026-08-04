import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  heading: {
    id: 'voting.governance.heading',
    defaultMessage: '!!!CARDANO VOTING POWER DELEGATION',
    description: 'Headline for Governance',
  },
  paragraph1: {
    id: 'voting.governance.paragraph1',
    defaultMessage: '!!!Governance first paragraph',
    description: 'First paragraph for governance page',
  },
  paragraph1LinkUrl: {
    id: 'voting.governance.paragraph1LinkUrl',
    defaultMessage: '!!!Governance first paragraph link url',
    description: 'First paragraph link for governance page',
  },
  paragraph1LinkText: {
    id: 'voting.governance.paragraph1LinkText',
    defaultMessage: '!!!Governance link label',
    description: 'Link labels for governance page',
  },
  selectWalletLabel: {
    id: 'voting.governance.selectWalletLabel',
    defaultMessage: '!!!Select a wallet to delegate from',
    description: 'Label for the wallet select on the governance page',
  },
  selectWalletPlaceholder: {
    id: 'voting.governance.selectWalletPlaceholder',
    defaultMessage: '!!!Select a wallet …',
    description: 'Placeholder for the wallet select on the governance page',
  },
  submitLabel: {
    id: 'voting.governance.submitLabel',
    defaultMessage: '!!!Submit',
    description: 'Label for the submit button on the governance page',
  },
  initializeTxErrorGeneric: {
    id: 'voting.governance.initializeTxError.generic',
    defaultMessage: '!!!Could not initialize transaction. Please try again!',
    description: 'Generic error for initialize transaction',
  },
  initializeTxErrorSameVote: {
    id: 'voting.governance.initializeTxError.sameVote',
    defaultMessage: '!!!Chosen same value as previously',
    description: 'Chosen same value as previously',
  },
  initializeNotEnoughMoney: {
    id: 'voting.governance.initializeTxError.notEnoughMoney',
    defaultMessage: '!!!Not enough funds',
    description: 'Governance voting error when wallet has not enough funds',
  },
  browseDRepsButton: {
    id: 'voting.governance.browseDRepsButton',
    defaultMessage: '!!!Browse DReps',
    description: 'Button that opens the DRep directory to pick a DRep',
  },
  selectedDRepHeading: {
    id: 'voting.governance.selectedDRepHeading',
    defaultMessage: '!!!Delegate to',
    description: 'Heading above the DRep card the user has selected to delegate to',
  },
  changeDRep: {
    id: 'voting.governance.changeDRep',
    defaultMessage: '!!!Change',
    description: 'Link that reopens the DRep directory to pick a different DRep',
  },
});

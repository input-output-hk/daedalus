import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'voting.governance.confirmationDialog.title',
    defaultMessage: '!!!Confirm Transaction',
    description: 'Title for confirm dialog',
  },
  vote: {
    id: 'voting.governance.confirmationDialog.vote',
    defaultMessage: '!!!Vote',
    description: 'Vote title',
  },
  drepId: {
    id: 'voting.governance.confirmationDialog.drepId',
    defaultMessage: '!!!DRep ID',
    description:
      'Label above the DRep ID in the delegation confirmation dialog',
  },
  fee: {
    id: 'voting.governance.confirmationDialog.fee',
    defaultMessage: '!!!Transaction fee',
    description: 'Fee title',
  },
  password: {
    id: 'voting.governance.confirmationDialog.password',
    defaultMessage: '!!!Spending password',
    description: 'Label for password input',
  },
  errorGeneric: {
    id: 'voting.governance.confirmationDialog.error.generic',
    defaultMessage:
      '!!!Something went wrong during transaction submission. Please try again in a few minutes.',
    description: 'Generic error message',
  },
  buttonCancel: {
    id: 'voting.governance.confirmationDialog.button.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Cancel button',
  },
  buttonConfirm: {
    id: 'voting.governance.confirmationDialog.button.confirm',
    defaultMessage: '!!!Confirm',
    description: 'Confirm button',
  },
});

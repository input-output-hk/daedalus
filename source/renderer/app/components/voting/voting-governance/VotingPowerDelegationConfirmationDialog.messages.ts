import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  vote: {
    id: 'voting.governance.confirmationDialog.vote',
    defaultMessage: '!!!Vote',
    description: 'Vote title',
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
  errorPassword: {
    id: 'voting.governance.confirmationDialog.error.password',
    defaultMessage: '!!!Wrong password, please try again',
    description: 'Wrong password error message',
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

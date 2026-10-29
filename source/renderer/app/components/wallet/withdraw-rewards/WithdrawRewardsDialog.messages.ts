import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  title: {
    id: 'wallet.withdrawRewards.dialog.title',
    defaultMessage: '!!!Withdraw staking rewards',
    description: 'Title of the dialog',
  },
  description: {
    id: 'wallet.withdrawRewards.dialog.description',
    defaultMessage:
      "!!!This transaction moves this wallet's staking rewards into the wallet and makes no payment.",
    description: 'Text at the top of the dialog',
  },
  amountLabel: {
    id: 'wallet.withdrawRewards.dialog.amountLabel',
    defaultMessage: '!!!Staking rewards',
    description: 'Label of the amount the transaction withdraws',
  },
  submit: {
    id: 'wallet.withdrawRewards.dialog.submit',
    defaultMessage: '!!!Withdraw',
    description: 'Submit button of the dialog',
  },
});

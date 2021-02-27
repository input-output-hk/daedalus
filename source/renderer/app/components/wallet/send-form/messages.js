// @flow
import { defineMessages } from 'react-intl';

export default defineMessages({
  titleLabel: {
    id: 'wallet.send.form.title.label',
    defaultMessage: '!!!Title',
    description: 'Label for the "Title" text input in the wallet send form.',
  },
  titleHint: {
    id: 'wallet.send.form.title.hint',
    defaultMessage: '!!!E.g: Money for Frank',
    description:
      'Hint inside the "Receiver" text input in the wallet send form.',
  },
  receiverLabel: {
    id: 'wallet.send.form.receiver.label',
    defaultMessage: '!!!Receiver',
    description: 'Label for the "Receiver" text input in the wallet send form.',
  },
  receiverHint: {
    id: 'wallet.send.form.receiver.placeholder',
    defaultMessage: '!!!Paste an address',
    description:
      'Hint inside the "Receiver" text input in the wallet send form.',
  },
  assetLabel: {
    id: 'wallet.send.form.asset.label',
    defaultMessage: '!!!Token',
    description: 'Label for the "Token" number input in the wallet send form.',
  },
  assetAdaLabel: {
    id: 'wallet.send.form.asset.adaLabel',
    defaultMessage: '!!!Ada',
    description: 'Label for the "Ada" input in the wallet send form.',
  },
  removeReceiverButtonLabel: {
    id: 'wallet.send.form.button.removeReceiver',
    defaultMessage: '!!!Remove',
    description: 'Label for the "Remove" button in the wallet send form.',
  },
  clearLabel: {
    id: 'wallet.send.form.button.clearLabel',
    defaultMessage: '!!!Clear',
    description: 'Label for the "Clear" button in the wallet send form.',
  },
  addAssetButtonLabel: {
    id: 'wallet.send.form.button.addAssetButtonLabel',
    defaultMessage: '!!!+ Add a token',
    description:
      'Label for the "+ Add a token" button in the wallet send form.',
  },
  estimatedFeeLabel: {
    id: 'wallet.send.form.estimatedFee.label',
    defaultMessage: '!!!Estimated fees',
    description:
      'Label for the "Estimated fees" number input in the wallet send form.',
  },
  ofLabel: {
    id: 'wallet.send.form.of.label',
    defaultMessage: '!!!of',
    description: 'Label for the "of" max ADA value in the wallet send form.',
  },
  minAdaRequired: {
    id: 'wallet.send.form.minAdaRequired',
    defaultMessage: '!!!a minimum of {minimumAda} ADA required',
    description:
      'Label for the min ADA required value in the wallet send form.',
  },
  minAdaRequiredTooltip: {
    id: 'wallet.send.form.minAdaRequiredTooltip',
    defaultMessage:
      '!!!A minimum of {minimumAda} ADA needs to be sent to this receiver since you are sending other assets.',
    description:
      'Tooltip for the min ADA required value in the wallet send form.',
  },
  descriptionLabel: {
    id: 'wallet.send.form.description.label',
    defaultMessage: '!!!Description',
    description:
      'Label for the "description" text area in the wallet send form.',
  },
  descriptionHint: {
    id: 'wallet.send.form.description.hint',
    defaultMessage: '!!!You can add a message if you want',
    description: 'Hint in the "description" text area in the wallet send form.',
  },
  resetButtonLabel: {
    id: 'wallet.send.form.reset',
    defaultMessage: '!!!Reset',
    description: 'Label for the reset button on the wallet send form.',
  },
  sendButtonLabel: {
    id: 'wallet.send.form.send',
    defaultMessage: '!!!Send',
    description: 'Label for the send button on the wallet send form.',
  },
  invalidAmount: {
    id: 'wallet.send.form.errors.invalidAmount',
    defaultMessage: '!!!Please enter a valid amount.',
    description: 'Error message shown when invalid amount was entered.',
  },
  invalidTitle: {
    id: 'wallet.send.form.errors.invalidTitle',
    defaultMessage: '!!!Please enter a title with at least 3 characters.',
    description:
      'Error message shown when invalid transaction title was entered.',
  },
  syncingTransactionsMessage: {
    id: 'wallet.send.form.syncingTransactionsMessage',
    defaultMessage:
      '!!!This wallet is currently being synced with the blockchain. While synchronisation is in progress transacting is not possible and transaction history is not complete.',
    description:
      'Syncing transactions message shown during async wallet restore in the wallet send form.',
  },
  calculatingFeesLabel: {
    id: 'wallet.send.form.calculatingFeesLabel',
    defaultMessage: '!!!Calculating fees',
    description:
      'Label for the "Calculating fees" message for amount input field.',
  },
  syncingWallet: {
    id: 'wallet.send.form.syncingWallet',
    defaultMessage: '!!!syncing',
    description: 'Syncing wallet label on the send tokens form.',
  },
});

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
  adaAmountLabel: {
    id: 'wallet.send.form.asset.adaLabel',
    defaultMessage: '!!!Ada',
    description: 'Label for the "Ada" input in the wallet send form.',
  },
  removeLabel: {
    id: 'wallet.send.form.button.removeLabel',
    defaultMessage: '!!!Remove',
    description: 'Label for the "Remove" button in the wallet send form.',
  },
  clearLabel: {
    id: 'wallet.send.form.button.clearLabel',
    defaultMessage: '!!!Clear',
    description: 'Label for the "Clear" button in the wallet send form.',
  },
  sameWalletLabel: {
    id: 'wallet.send.form.sameWalletLabel',
    defaultMessage:
      '!!!This receiver address belongs to the same wallet from which you are sending funds. If you proceed with this transaction, the transferred funds will remain in this wallet, and you will incur transaction fees, as outlined in Estimated fees.',
    description: 'Label for the same wallet tooltip in the wallet send form.',
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
  minAdaRequiredWithAssetTooltip: {
    id: 'wallet.send.form.minAdaRequiredWithAssetTooltip',
    defaultMessage:
      '!!!This transaction requires a minimum of {minimumAda} ADA to be sent.',
    description:
      'Tooltip for the min ADA required value in the wallet send form.',
  },
  minAdaRequiredWithNoAssetTooltip: {
    id: 'wallet.send.form.minAdaRequiredWithNoAssetTooltip',
    defaultMessage:
      '!!!A minimum of {minimumAda} ADA needs to be sent with every transaction.',
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
      '!!!The balance and transaction history of this wallet is being synced with the blockchain.',
    description:
      'Syncing transactions message shown during async wallet restore in the wallet send form.',
  },
  calculatingFeesLabel: {
    id: 'wallet.send.form.calculatingFeesLabel',
    defaultMessage: '!!!Calculating fees',
    description:
      'Label for the "Calculating fees" message for amount input field.',
  },
  updateAdaAmountButton: {
    id: 'wallet.send.form.asset.updateAdaAmountButton',
    defaultMessage: 'UPDATE',
    description:
      'Label for the "UPDATE" button responsible to set minimum amount required for transaction .',
  },
  updateAdaAmountDescription: {
    id: 'wallet.send.form.updateAdaAmountDescription',
    defaultMessage: 'to the minimum of {minimumAda} ADA required',
    description:
      'Description for the "UPDATE" button when ADA amount is less than required ',
  },
  minimumAmountNotice: {
    id: 'wallet.send.form.minimumAmountNote',
    defaultMessage:
      '<b>Note:</b> the ada field was updated because this transaction requires a minimum of {minimumAda} ADA.',
    description: 'Minimum amount update notice',
  },
});

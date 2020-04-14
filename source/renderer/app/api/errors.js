import { defineMessages } from 'react-intl';

const messages = defineMessages({
  // wallets
  walletAlreadyExists: {
    id: 'api.errors.WalletAlreadyRestoredError',
    defaultMessage: '!!!Wallet you are trying to restore already exists.',
    description:
      '"Wallet you are trying to restore already exists." error message.',
  },
  // common
  wrongEncryptionPassphrase: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.',
  },
  // transactions
  notEnoughMoney: {
    id: 'api.errors.NotEnoughMoneyToSendError',
    defaultMessage: '!!!Not enough money to make this transaction.',
    description: '"Not enough money to make this transaction." error message.',
  },
  canNotCalculateTransactionFees: {
    id: 'api.errors.CanNotCalculateTransactionFeesError',
    defaultMessage:
      '!!!Cannot calculate fees while there are pending transactions.',
    description:
      '"Cannot calculate fees while there are pending transactions." error message',
  },
  cannotCoverFee: {
    id: 'api.errors.NotEnoughFundsForTransactionFeesError',
    defaultMessage: '!!!Not enough ada for fees. Try sending a smaller amount.',
    description:
      '"Not enough ada for fees. Try sending a smaller amount." error message',
  },
  transactionIsTooBig: {
    id: 'api.errors.TooBigTransactionError',
    defaultMessage: '!!!Transaction too big due to too many inputs.',
    description: '"Transaction too big due to too many inputs." error message.',
  },

  // Additional
  notEnoughFundsForTransaction: {
    id: 'api.errors.NotEnoughFundsForTransactionError',
    defaultMessage: '!!!Not enough ada . Try sending a smaller amount.',
    description:
      '"Not enough ada . Try sending a smaller amount." error message',
  },
  invalidAddress: {
    id: 'api.errors.invalidAddress',
    defaultMessage: '!!!Please enter a valid address.',
    description: 'Error message shown when invalid address was entered.',
  },
  forbiddenMnemonic: {
    id: 'api.errors.ForbiddenMnemonicError',
    defaultMessage:
      '!!!Invalid recovery phrase. Submitted recovery phrase is one of the example recovery phrases from the documentation and should not be used for wallets holding funds.',
    description:
      '"Forbidden Mnemonic: an example Mnemonic has been submitted." error message',
  },
  walletAlreadyImported: {
    id: 'api.errors.WalletAlreadyImportedError',
    defaultMessage: '!!!Wallet you are trying to import already exists.',
    description:
      '"Wallet you are trying to import already exists." error message.',
  },
  walletFileImportError: {
    id: 'api.errors.WalletFileImportError',
    defaultMessage:
      '!!!Wallet could not be imported, please make sure you are providing a correct file.',
    description:
      '"Wallet could not be imported, please make sure you are providing a correct file." error message.',
  },
});

export default messages;

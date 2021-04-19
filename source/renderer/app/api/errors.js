import { defineMessages } from 'react-intl';

export const messages = defineMessages({
  // common
  wrongEncryptionPassphrase: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.',
  },
  // wallets
  walletAlreadyExists: {
    id: 'api.errors.WalletAlreadyRestoredError',
    defaultMessage: '!!!Wallet you are trying to restore already exists.',
    description:
      '"Wallet you are trying to restore already exists." error message.',
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
  invalidMnemonic: {
    id: 'global.errors.invalidMnemonic',
    defaultMessage: '!!!Invalid phrase entered, please check.',
    description: 'Error message shown when invalid bip39 mnemonic was entered.',
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
  inputsDepleted: {
    id: 'api.errors.inputsDepleted',
    defaultMessage:
      '!!!Your wallet contains only reward funds. Please send at least one ADA to your wallet so that you can spend the funds.',
    description:
      '"Your wallet contains only reward funds. Please send at least one ADA to your wallet so that you can spend the funds." error message',
  },
  transactionIsTooBig: {
    id: 'api.errors.TooBigTransactionError',
    defaultMessage: '!!!Transaction too big due to too many inputs.',
    description: '"Transaction too big due to too many inputs." error message.',
  },
  notEnoughFundsForTransaction: {
    id: 'api.errors.NotEnoughFundsForTransactionError',
    defaultMessage: '!!!Not enough ada. Try sending a smaller amount.',
    description:
      '"Not enough ada . Try sending a smaller amount." error message',
  },
  invalidAddress: {
    id: 'api.errors.invalidAddress',
    defaultMessage: '!!!Please enter a valid address.',
    description: 'Error message shown when invalid address was entered.',
  },
  tooBigTransactionErrorLinkLabel: {
    id: 'api.errors.TooBigTransactionErrorLinkLabel',
    defaultMessage: '!!!Learn more.',
    description:
      '"Transaction too big due to too many inputs." error link label.',
  },
  tooBigTransactionErrorLinkURL: {
    id: 'api.errors.TooBigTransactionErrorLinkURL',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360017733353',
    description:
      '"Transaction too big due to too many inputs." error link URL.',
  },
  utxoTooSmall: {
    id: 'api.errors.utxoTooSmall',
    defaultMessage: '!!!Invalid transaction.',
    description: '"Invalid transaction." error message',
  },
  nothingToMigrate: {
    id: 'api.errors.nothingToMigrate',
    defaultMessage:
      '!!!Funds cannot be transferred from this wallet because it contains some unspent transaction outputs (UTXOs), with amounts of ada that are too small to be migrated.',
    description:
      '"Funds cannot be transferred from this wallet because it contains some unspent transaction outputs (UTXOs), with amounts of ada that are too small to be migrated." error message',
  },
  invalidSmashServer: {
    id: 'api.errors.invalidSmashServer',
    defaultMessage: '!!!This URL is not a valid SMASH server',
    description: '"This URL is not a valid SMASH server" error message',
  },
});

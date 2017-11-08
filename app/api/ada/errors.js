import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';

const messages = defineMessages({
  apiMethodNotYetImplementedError: {
    id: 'api.errors.ApiMethodNotYetImplementedError',
    defaultMessage: '!!!This API method is not yet implemented.',
    description: '"This API method is not yet implemented." error message.'
  },
  walletAlreadyImportedError: {
    id: 'api.errors.WalletAlreadyImportedError',
    defaultMessage: '!!!Wallet you are trying to import already exists.',
    description: '"Wallet you are trying to import already exists." error message.'
  },
  redeemAdaError: {
    id: 'api.errors.RedeemAdaError',
    defaultMessage: '!!!Your ADA could not be redeemed correctly.',
    description: '"Your ADA could not be redeemed correctly." error message.'
  },
  walletFileImportError: {
    id: 'api.errors.WalletFileImportError',
    defaultMessage: '!!!Wallet could not be imported, please make sure you are providing a correct file.',
    description: '"Wallet could not be imported, please make sure you are providing a correct file." error message.'
  },
  notEnoughMoneyToSendError: {
    id: 'api.errors.NotEnoughMoneyToSendError',
    defaultMessage: '!!!Not enough money to make this transaction.',
    description: '"Not enough money to make this transaction." error message.'
  },
  notAllowedToSendMoneyToSameAddressError: {
    id: 'api.errors.NotAllowedToSendMoneyToSameAddressError',
    defaultMessage: '!!!It\'s not allowed to send money to the same address you are sending from. Make sure you have enough addresses with money in this account or send to a different address.',
    description: '"It\'s not allowed to send money to the same address you are sending from." error message.'
  },
  notAllowedToSendMoneyToRedeemAddressError: {
    id: 'api.errors.NotAllowedToSendMoneyToRedeemAddressError',
    defaultMessage: '!!!It is not allowed to send money to Ada redemption address.',
    description: '"It is not allowed to send money to Ada redemption address." error message.'
  },
  allFundsAlreadyAtReceiverAddressError: {
    id: 'api.errors.AllFundsAlreadyAtReceiverAddressError',
    defaultMessage: '!!!All your funds are already at the address you are trying send money to.',
    description: '"All your funds are already at the address you are trying send money to." error message.'
  },
  notEnoughFundsForTransactionFeesError: {
    id: 'api.errors.NotEnoughFundsForTransactionFeesError',
    defaultMessage: '!!!Not enough Ada for fees. Try sending a smaller amount.',
    description: '"Not enough Ada for fees. Try sending a smaller amount." error message'
  },
});

export class ApiMethodNotYetImplementedError extends LocalizableError {
  constructor() {
    super({
      id: messages.apiMethodNotYetImplementedError.id,
      defaultMessage: messages.apiMethodNotYetImplementedError.defaultMessage,
    });
  }
}

export class WalletAlreadyImportedError extends LocalizableError {
  constructor() {
    super({
      id: messages.walletAlreadyImportedError.id,
      defaultMessage: messages.walletAlreadyImportedError.defaultMessage,
    });
  }
}

export class RedeemAdaError extends LocalizableError {
  constructor() {
    super({
      id: messages.redeemAdaError.id,
      defaultMessage: messages.redeemAdaError.defaultMessage,
    });
  }
}

export class WalletFileImportError extends LocalizableError {
  constructor() {
    super({
      id: messages.walletFileImportError.id,
      defaultMessage: messages.walletFileImportError.defaultMessage,
    });
  }
}

export class NotEnoughMoneyToSendError extends LocalizableError {
  constructor() {
    super({
      id: messages.notEnoughMoneyToSendError.id,
      defaultMessage: messages.notEnoughMoneyToSendError.defaultMessage,
    });
  }
}

export class NotAllowedToSendMoneyToSameAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.notAllowedToSendMoneyToSameAddressError.id,
      defaultMessage: messages.notAllowedToSendMoneyToSameAddressError.defaultMessage,
    });
  }
}

export class NotAllowedToSendMoneyToRedeemAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.notAllowedToSendMoneyToRedeemAddressError.id,
      defaultMessage: messages.notAllowedToSendMoneyToRedeemAddressError.defaultMessage,
    });
  }
}

export class AllFundsAlreadyAtReceiverAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.allFundsAlreadyAtReceiverAddressError.id,
      defaultMessage: messages.allFundsAlreadyAtReceiverAddressError.defaultMessage,
    });
  }
}

export class NotEnoughFundsForTransactionFeesError extends LocalizableError {
  constructor() {
    super({
      id: messages.notEnoughFundsForTransactionFeesError.id,
      defaultMessage: messages.notEnoughFundsForTransactionFeesError.defaultMessage,
    });
  }
}

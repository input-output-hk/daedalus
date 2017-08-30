import { defineMessages } from 'react-intl';
import LocalizableError from '../i18n/LocalizableError';

const messages = defineMessages({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred, please try again later.',
    description: 'Generic error message.'
  },
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
  walletAlreadyRestoredError: {
    id: 'api.errors.WalletAlreadyRestoredError',
    defaultMessage: '!!!Wallet you are trying to restore already exists.',
    description: '"Wallet you are trying to restore already exists." error message.'
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
  incorrectWalletPasswordError: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.'
  },
});

export class GenericApiError extends LocalizableError {
  constructor() {
    super({
      id: messages.genericApiError.id,
      defaultMessage: messages.genericApiError.defaultMessage,
    });
  }
}

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

export class WalletAlreadyRestoredError extends LocalizableError {
  constructor() {
    super({
      id: messages.walletAlreadyRestoredError.id,
      defaultMessage: messages.walletAlreadyRestoredError.defaultMessage,
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

export class IncorrectWalletPasswordError extends LocalizableError {
  constructor() {
    super({
      id: messages.incorrectWalletPasswordError.id,
      defaultMessage: messages.incorrectWalletPasswordError.defaultMessage,
    });
  }
}

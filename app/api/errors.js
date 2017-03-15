import LocalizableError from '../i18n/LocalizableError';

export class GenericApiError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.GenericApiError',
      defaultMessage: '!!!An error occurred, please try again later.',
    });
  }
}

export class ApiMethodNotYetImplementedError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.ApiMethodNotYetImplementedError',
      defaultMessage: '!!!This API method is not yet implemented.',
    });
  }
}

export class WalletAlreadyRestoredError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.WalletAlreadyRestoredError',
      defaultMessage: '!!!You already restored a wallet with this phrase.',
    });
  }
}

export class RedeemAdaError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.RedeemAdaError',
      defaultMessage: '!!!Your ADA could not be redeemed correctly.',
    });
  }
}

export class WalletKeyImportError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.WalletKeyImportError',
      defaultMessage: '!!!Key could not be imported, please make sure you are providing a correct key file.',
    });
  }
}

export class NotEnoughMoneyToSendError extends LocalizableError {
  constructor() {
    super({
      id: 'api.errors.NotEnoughMoneyToSendError',
      defaultMessage: '!!!Not enough money to make this transaction.',
    });
  }
}

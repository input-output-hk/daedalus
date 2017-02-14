import LocalizableError from '../i18n/LocalizableError';

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

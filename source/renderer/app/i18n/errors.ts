import LocalizableError from './LocalizableError';

export class InvalidMnemonicError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.invalidMnemonic',
      defaultMessage: '!!!Invalid phrase entered, please check.',
    });
  }
}
export class InvalidEmailError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.invalidEmail',
      defaultMessage: '!!!Invalid email entered, please check.',
    });
  }
}
export class FieldRequiredError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.fieldIsRequired',
      defaultMessage: '!!!This field is required.',
    });
  }
}
export class WalletSupportRequestLogsCompressError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.WalletSupportRequestLogsCompressError',
      defaultMessage: '!!!Compressing logs failed, please try again.',
    });
  }
}
export class WalletPaperWalletOpenPdfError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.paperWalletOpenPdfError',
      defaultMessage:
        '!!!The file you are trying to replace is open. Please close it and try again.',
    });
  }
}
export class WalletRewardsOpenCsvError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.rewardsOpenCsvError',
      defaultMessage:
        '!!!The file you are trying to replace is open. Please close it and try again.',
    });
  }
}

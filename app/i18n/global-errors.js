import LocalizableError from './LocalizableError';

export class InvalidMnemonicError extends LocalizableError {
  constructor() {
    super({
      id: 'global.errors.invalidMnemonic',
      defaultMessage: '!!!Invalid phrase entered, please check.',
    });
  }
}

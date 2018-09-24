import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';
import globalMessages from '../../i18n/global-messages';

const messages = defineMessages({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred, please try again later.',
    description: 'Generic error message.'
  },
  incorrectWalletPasswordError: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.'
  },
  reportRequestError: {
    id: 'api.errors.ReportRequestError',
    defaultMessage: '!!!There was a problem sending the support request.',
    description: '"There was a problem sending the support request." error message'
  },
  apiMethodNotYetImplementedError: {
    id: 'api.errors.ApiMethodNotYetImplementedError',
    defaultMessage: '!!!This API method is not yet implemented.',
    description: '"This API method is not yet implemented." error message.'
  },
  forbiddenMnemonicError: {
    id: 'api.errors.ForbiddenMnemonicError',
    defaultMessage: '!!!Forbidden Mnemonic: an example Mnemonic has been submitted. Please generate a fresh and private Mnemonic from a trusted source.',
    description: '"Forbidden Mnemonic: an example Mnemonic has been submitted." error message',
  },
});

export class GenericApiError extends LocalizableError {
  constructor(values?: Object = {}) {
    super({
      id: messages.genericApiError.id,
      defaultMessage: messages.genericApiError.defaultMessage,
      values,
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

export class ReportRequestError extends LocalizableError {
  constructor() {
    super({
      id: messages.reportRequestError.id,
      defaultMessage: messages.reportRequestError.defaultMessage,
    });
  }
}

export class ForbiddenMnemonicError extends LocalizableError {
  constructor() {
    super({
      id: messages.forbiddenMnemonicError.id,
      defaultMessage: messages.forbiddenMnemonicError.defaultMessage,
    });
  }
}

export class InvalidMnemonicError extends LocalizableError {
  constructor() {
    super({
      id: globalMessages.invalidMnemonic.id,
      defaultMessage: globalMessages.invalidMnemonic.defaultMessage,
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

// @flow
import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';
import globalMessages from '../../i18n/global-messages';

const messages = defineMessages({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred.',
    description: 'Generic error message.',
  },
  incorrectSpendingPasswordError: {
    id: 'api.errors.IncorrectPasswordError',
    defaultMessage: '!!!Incorrect wallet password.',
    description: '"Incorrect wallet password." error message.',
  },
  reportRequestError: {
    id: 'api.errors.ReportRequestError',
    defaultMessage: '!!!There was a problem sending the support request.',
    description:
      '"There was a problem sending the support request." error message',
  },
  apiMethodNotYetImplementedError: {
    id: 'api.errors.ApiMethodNotYetImplementedError',
    defaultMessage: '!!!This API method is not yet implemented.',
    description: '"This API method is not yet implemented." error message.',
  },
  forbiddenMnemonicError: {
    id: 'api.errors.ForbiddenMnemonicError',
    defaultMessage:
      '!!!Invalid recovery phrase. Submitted recovery phrase is one of the example recovery phrases from the documentation and should not be used for wallets holding funds.',
    description:
      '"Forbidden Mnemonic: an example Mnemonic has been submitted." error message',
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

export class IncorrectSpendingPasswordError extends LocalizableError {
  constructor() {
    super({
      id: messages.incorrectSpendingPasswordError.id,
      defaultMessage: messages.incorrectSpendingPasswordError.defaultMessage,
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

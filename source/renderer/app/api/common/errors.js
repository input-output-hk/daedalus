// @flow
import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';

export const messages = defineMessages({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred.',
    description: 'Generic error message.',
  },
  apiMethodNotYetImplementedError: {
    id: 'api.errors.ApiMethodNotYetImplementedError',
    defaultMessage: '!!!This API method is not yet implemented.',
    description: '"This API method is not yet implemented." error message.',
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

export class ApiMethodNotYetImplementedError extends LocalizableError {
  constructor() {
    super({
      id: messages.apiMethodNotYetImplementedError.id,
      defaultMessage: messages.apiMethodNotYetImplementedError.defaultMessage,
    });
  }
}

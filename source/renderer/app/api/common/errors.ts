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
  // @ts-ignore ts-migrate(1015) FIXME: Parameter cannot have question mark and initialize... Remove this comment to see the full error message
  constructor(values?: Record<string, any> = {}) {
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

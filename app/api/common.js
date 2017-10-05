import {defineMessages} from 'react-intl';
import LocalizableError from '../i18n/LocalizableError';

const messages = defineMessages({
  genericApiError: {
    id: 'api.errors.GenericApiError',
    defaultMessage: '!!!An error occurred, please try again later.',
    description: 'Generic error message.'
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

export type GetSyncProgressResponse = {
  localDifficulty: ?number,
  networkDifficulty: ?number,
};

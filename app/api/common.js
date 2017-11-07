import { defineMessages } from 'react-intl';
import LocalizableError from '../i18n/LocalizableError';
import WalletTransaction from '../domain/WalletTransaction';

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
});

export class GenericApiError extends LocalizableError {
  constructor() {
    super({
      id: messages.genericApiError.id,
      defaultMessage: messages.genericApiError.defaultMessage,
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

export type GetSyncProgressResponse = {
  localDifficulty: ?number,
  networkDifficulty: ?number,
};

export type GetWalletRecoveryPhraseResponse = Array<string>;

export type GetTransactionsResponse = {
  transactions: Array<WalletTransaction>,
  total: number,
};

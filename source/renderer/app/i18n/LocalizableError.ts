import ExtendableError from 'es6-error';
import type { ReactIntlMessageShape } from './types';

type KnownErrorType = 'api.errors.NotEnoughFundsForTransactionFeesErrorWithTokens';

export default class LocalizableError extends ExtendableError {
  id: KnownErrorType | string;
  defaultMessage: string;
  values?: Record<string, any>;
  constructor({ id, defaultMessage, values = {} }: ReactIntlMessageShape) {
    if (!id) throw new Error('id:string is required.');
    if (!defaultMessage) throw new Error('defaultMessage:string is required.');
    super(`${id}: ${JSON.stringify(values)}`);
    this.id = id;
    this.defaultMessage = defaultMessage;
    this.values = values;
  }
}

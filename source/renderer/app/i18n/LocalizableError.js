// @flow
import ExtendableError from 'es6-error';

import type { ReactIntlMessageShape } from './types';

export default class LocalizableError extends ExtendableError {
  constructor({ id, defaultMessage, values = {} }: ReactIntlMessageShape) {
    if (!id) throw new Error('id:string is required.');
    if (!defaultMessage) throw new Error('defaultMessage:string is required.');
    super(`${id}: ${JSON.stringify(values)}`);
    this.id = id;
    this.defaultMessage = defaultMessage;
    this.values = values;
  }
}

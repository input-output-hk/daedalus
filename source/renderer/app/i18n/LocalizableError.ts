import ExtendableError from 'es6-error';
import type { ReactIntlMessageShape } from './types';

export default class LocalizableError extends ExtendableError {
  constructor({ id, defaultMessage, values = {} }: ReactIntlMessageShape) {
    if (!id) throw new Error('id:string is required.');
    if (!defaultMessage) throw new Error('defaultMessage:string is required.');
    super(`${id}: ${JSON.stringify(values)}`);
    // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'LocalizableE... Remove this comment to see the full error message
    this.id = id;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'defaultMessage' does not exist on type '... Remove this comment to see the full error message
    this.defaultMessage = defaultMessage;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'values' does not exist on type 'Localiza... Remove this comment to see the full error message
    this.values = values;
  }
}

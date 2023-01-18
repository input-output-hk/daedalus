import Request from './Request';
import LocalizableError from '../../i18n/LocalizableError';

export default class LocalizedRequest<Result> extends Request<
  Result,
  LocalizableError
> {}

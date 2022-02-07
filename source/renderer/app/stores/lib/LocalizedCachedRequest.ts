import CachedRequest from './CachedRequest';
import LocalizableError from '../../i18n/LocalizableError'; // eslint-disable-next-line

export default class LocalizedCachedRequest<Result> extends CachedRequest<
  Result,
  LocalizableError
> {}

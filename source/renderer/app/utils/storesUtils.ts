import { findKey, camelCase } from 'lodash';
import { LOCALES } from '../../../common/types/locales.types';
import Request from '../stores/lib/LocalizedRequest';
import type { Locale } from '../../../common/types/locales.types';

export const getRequestKeys = (
  param: string,
  currentLocale: Locale
): Request<string> => {
  const currentLanguage = findKey(LOCALES, (l) => l === currentLocale);
  const languageSufix = param === 'dateFormat' ? currentLanguage : '';
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
  const requestKey = camelCase(['Profile', param, languageSufix, 'Request']);
  return {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ set: string; get: string; }' is not assign... Remove this comment to see the full error message
    set: camelCase(['set', requestKey]),
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
    get: camelCase(['get', requestKey]),
  };
};
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
export const requestGetter = (req: Request, systemValue: any) => {
  const { result } = req.execute();
  if (isRequestSet(req)) return result;
  return systemValue;
};
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
export const requestGetterLocale = (req: Request, systemValue: Locale) => {
  const { result } = req.execute();
  if (isRequestSet(req)) return result;
  return systemValue;
};
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
export const hasLoadedRequest = (req: Request) =>
  req.wasExecuted && req.result !== null;
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
export const isRequestSet = (req: Request) =>
  req.result !== null && req.result !== '';

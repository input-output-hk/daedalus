// @flow
import { findKey, camelCase } from 'lodash';
import { LOCALES } from '../../../common/types/locales.types';
import Request from '../stores/lib/LocalizedRequest';

export const getRequestKeys = (
  param: string,
  currentLocale: string
): Request<string> => {
  const currentLanguage = findKey(LOCALES, l => l === currentLocale);
  const languageSufix = param === 'dateFormat' ? currentLanguage : '';
  const requestKey = camelCase(['Profile', param, languageSufix, 'Request']);
  return {
    set: camelCase(['set', requestKey]),
    get: camelCase(['get', requestKey]),
  };
};

export const requestGetter = (req: Request, systemValue: string) => {
  const { result } = req.execute();
  if (isRequestSet(req)) return result;
  return systemValue;
};

export const hasLoadedRequest = (req: Request) =>
  req.wasExecuted && req.result !== null;

export const isRequestSet = (req: Request) =>
  req.result !== null && req.result !== '';

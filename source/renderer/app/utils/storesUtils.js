'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.isRequestSet = exports.hasLoadedRequest = exports.requestGetterLocale = exports.requestGetter = exports.getRequestKeys = void 0;
const lodash_1 = require('lodash');
const locales_types_1 = require('../../../common/types/locales.types');
const getRequestKeys = (param, currentLocale) => {
  const currentLanguage = (0, lodash_1.findKey)(
    locales_types_1.LOCALES,
    (l) => l === currentLocale
  );
  const languageSufix = param === 'dateFormat' ? currentLanguage : '';
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
  const requestKey = (0, lodash_1.camelCase)([
    'Profile',
    param,
    languageSufix,
    'Request',
  ]);
  return {
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ set: string; get: string; }' is not assign... Remove this comment to see the full error message
    set: (0, lodash_1.camelCase)(['set', requestKey]),
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
    get: (0, lodash_1.camelCase)(['get', requestKey]),
  };
};
exports.getRequestKeys = getRequestKeys;
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
const requestGetter = (req, systemValue) => {
  const { result } = req.execute();
  if ((0, exports.isRequestSet)(req)) return result;
  return systemValue;
};
exports.requestGetter = requestGetter;
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
const requestGetterLocale = (req, systemValue) => {
  const { result } = req.execute();
  if ((0, exports.isRequestSet)(req)) return result;
  return systemValue;
};
exports.requestGetterLocale = requestGetterLocale;
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
const hasLoadedRequest = (req) => req.wasExecuted && req.result !== null;
exports.hasLoadedRequest = hasLoadedRequest;
// @ts-ignore ts-migrate(2314) FIXME: Generic type 'LocalizedRequest<Result>' requires 1... Remove this comment to see the full error message
const isRequestSet = (req) => req.result !== null && req.result !== '';
exports.isRequestSet = isRequestSet;
//# sourceMappingURL=storesUtils.js.map

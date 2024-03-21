'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.getCurrencyFromCode = exports.getLocalizedCurrency = exports.getLocalizedCurrenciesList = exports.genericCurrencyRequest = exports.CURRENCY_REQUEST_RATE_INTERVAL = exports.CURRENCY_DEFAULT_SELECTED = exports.CURRENCY_IS_ACTIVE_BY_DEFAULT = exports.currencyConfig = exports.REQUESTS = void 0;
/**
 *
 * This file imports the external currency API used
 *
 */
const lodash_1 = require('lodash');
// Available APIS
const currencyConfig_coingecko_1 = __importDefault(
  require('./currencyConfig.coingecko')
);
const externalRequest_1 = require('../api/utils/externalRequest');
const currenciesList_json_1 = __importDefault(require('./currenciesList.json'));
const locales_types_1 = require('../../../common/types/locales.types');
exports.REQUESTS = {
  LIST: 'list',
  RATE: 'rate',
};
// Definitions
exports.currencyConfig = currencyConfig_coingecko_1.default;
exports.CURRENCY_IS_ACTIVE_BY_DEFAULT = true;
exports.CURRENCY_DEFAULT_SELECTED = currenciesList_json_1.default.usd;
exports.CURRENCY_REQUEST_RATE_INTERVAL = 60 * 1000; // 1 minute | unit: milliseconds
// Generic function for all the Currency requests
const genericCurrencyRequest = (
  requestName
  // @ts-ignore ts-migrate(1064) FIXME: The return type of an async function or method mus... Remove this comment to see the full error message
) => async (payload) => {
  const request = exports.currencyConfig.requests[requestName];
  let response;
  if (Array.isArray(request)) {
    response = [];
    for (const req of request) {
      const responseItem = await (0, externalRequest_1.externalRequest)(req);
      response.push(responseItem);
    }
  } else if (typeof request === 'function') {
    const req = request(payload);
    response = await (0, externalRequest_1.externalRequest)(req);
  } else if (request) {
    response = await (0, externalRequest_1.externalRequest)(request);
  }
  return response;
};
exports.genericCurrencyRequest = genericCurrencyRequest;
const getLocalizedCurrenciesList = (rawCurrencyList, currentLocale) =>
  (0, lodash_1.map)(rawCurrencyList, (rawCurrency) =>
    (0, exports.getLocalizedCurrency)(rawCurrency, currentLocale)
  );
exports.getLocalizedCurrenciesList = getLocalizedCurrenciesList;
const getLocalizedCurrency = (rawCurrency, currentLocale) => ({
  ...(0, lodash_1.omit)(rawCurrency, ['name']),
  name:
    rawCurrency.name[currentLocale] ||
    rawCurrency.name[locales_types_1.LOCALES.english],
});
exports.getLocalizedCurrency = getLocalizedCurrency;
const getCurrencyFromCode = (code) => currenciesList_json_1.default[code];
exports.getCurrencyFromCode = getCurrencyFromCode;
//# sourceMappingURL=currencyConfig.js.map

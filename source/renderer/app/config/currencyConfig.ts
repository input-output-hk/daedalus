/**
 *
 * This file imports the external currency API used
 *
 */
import { omit, map } from 'lodash';
// Available APIS
import coingeckoConfig from './currencyConfig.coingecko';
import { externalRequest } from '../api/utils/externalRequest';
import currenciesList from './currenciesList.json';
import { LOCALES } from '../../../common/types/locales.types';
import type {
  RequestName,
  Currency,
  LocalizedCurrency,
} from '../types/currencyTypes';
import type { Locale } from '../../../common/types/locales.types';

export const REQUESTS: Record<string, RequestName> = {
  LIST: 'list',
  RATE: 'rate',
};
// Definitions
export const currencyConfig = coingeckoConfig;
export const CURRENCY_IS_ACTIVE_BY_DEFAULT = true;
export const CURRENCY_DEFAULT_SELECTED = currenciesList.usd;
export const CURRENCY_REQUEST_RATE_INTERVAL = 60 * 1000; // 1 minute | unit: milliseconds

// Generic function for all the Currency requests
export const genericCurrencyRequest = (
  requestName: RequestName
  // @ts-ignore ts-migrate(1064) FIXME: The return type of an async function or method mus... Remove this comment to see the full error message
): ((...args: Array<any>) => any) => async (payload?: any): any => {
  const request = currencyConfig.requests[requestName];
  let response;

  if (Array.isArray(request)) {
    response = [];

    for (const req of request) {
      const responseItem = await externalRequest(req);
      response.push(responseItem);
    }
  } else if (typeof request === 'function') {
    const req = request(payload);
    response = await externalRequest(req);
  } else if (request) {
    response = await externalRequest(request);
  }

  return response;
};
export const getLocalizedCurrenciesList = (
  rawCurrencyList: Array<Currency>,
  currentLocale: Locale
) =>
  map(rawCurrencyList, (rawCurrency) =>
    getLocalizedCurrency(rawCurrency, currentLocale)
  );
export const getLocalizedCurrency = (
  rawCurrency: Currency,
  currentLocale: Locale
): LocalizedCurrency => ({
  ...omit(rawCurrency, ['name']),
  name: rawCurrency.name[currentLocale] || rawCurrency.name[LOCALES.english],
});
export const getCurrencyFromCode = (code: string): Currency =>
  currenciesList[code];

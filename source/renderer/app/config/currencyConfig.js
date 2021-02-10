// @flow

/**
 *
 * This file imports the external currency API used
 *
 */

// Available APIS
import coingeckoConfig from './currencyConfig.coingecko';
// import nomicsConfig from './currencyConfig.nomics';

import { externalRequest } from '../api/utils/externalRequest';
import currenciesList from './currenciesList.json';
import type { RequestName } from '../types/currencyTypes';

export const REQUESTS: {
  [key: string]: RequestName,
} = {
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
): Function => async (payload?: any): any => {
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

// @flow

export const CURRENCY_IS_ACTIVE_BY_DEFAULT = true;

/**
 *
 * This file imports the external currency API used
 *
 */

// Available APIS
import coingeckoConfig from './currencyConfig.coingecko';
// import nomicsConfig from './currencyConfig.nomics';

import { externalRequest } from '../api/utils/externalRequest';
import type { Request, RequestName } from '../types/currencyTypes';

export const REQUESTS: {
  [key: string]: RequestName,
} = {
  STATUS: 'status',
  LIST: 'list',
  RATE: 'rate',
};

// Defines which API is going to be used
export const currencyConfig = coingeckoConfig;

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
  } else {
    response = await externalRequest(request);
  }
  return response;
};

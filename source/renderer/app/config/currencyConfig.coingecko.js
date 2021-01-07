// @flow

/**
 *
 * CoingGecko API
 *
 * https://www.coingecko.com/en/api
 *
 * check `currencyConfig.js` for more info
 *
 */

import { logger } from '../utils/logging';
import type { Currency, CurrencyApiConfig } from '../types/currencyTypes.js';
import type {
  GetCurrencyApiStatusResponse,
  GetCurrencyListResponse,
  GetCurrencyRateResponse,
} from '../api/wallets/types';

// For the complete response, check
// https://api.coingecko.com/api/v3/coins/markets?ids=cardano&vs_currency=usd
type CurrencyRateGeckoResponse = Array<{
  current_price: number,
}>;

const id = 'coingecko';
const hostname = 'api.coingecko.com';
const version = 'v3';
const pathBase = `api/${version}`;

const requests = {
  status: {
    hostname,
    method: 'GET',
    path: `/${pathBase}/ping`,
  },
  list: [
    {
      hostname,
      method: 'GET',
      path: `/${pathBase}/coins/list`,
    },
    {
      hostname,
      method: 'GET',
      path: `/${pathBase}/simple/supported_vs_currencies`,
    },
  ],
  rate: ({ symbol }: Currency) => ({
    hostname,
    method: 'GET',
    path: `/${pathBase}/coins/markets?ids=cardano&vs_currency=${symbol}`,
  }),
};

const responses = {
  status: (): GetCurrencyApiStatusResponse => true,
  list: (apiResponse: Array<Object>): GetCurrencyListResponse => {
    try {
      if (!Array.isArray(apiResponse) || apiResponse.length < 2) {
        throw new Error('unexpected API response');
      }
      const [completeList, vsCurrencies] = apiResponse;
      const list = vsCurrencies
        .map((symbol) =>
          completeList.find((currency) => currency.symbol === symbol)
        )
        .filter((item) => !!item);
      logger.debug('Currency::CoingGecko::List success', { list });
      return list;
    } catch (error) {
      logger.error('Currency::CoingGecko::List error', { error });
      return null;
    }
  },
  rate: (apiResponse: CurrencyRateGeckoResponse): GetCurrencyRateResponse =>
    apiResponse[0].current_price,
};

export default ({
  id,
  hostname,
  requests,
  responses,
}: CurrencyApiConfig);

/**
 *
 * CoingGecko API
 *
 * https://www.coingecko.com/en/api
 *
 * check `currencyConfig.js` for more info
 *
 */
import { get } from 'lodash';
import { logger } from '../utils/logging';
import type {
  LocalizedCurrency,
  CurrencyApiConfig,
} from '../types/currencyTypes';
import type {
  GetCurrencyListResponse,
  GetCurrencyRateResponse,
} from '../api/wallets/types';
import currenciesList from './currenciesList.json';
// For the complete response, check
// https://api.coingecko.com/api/v3/coins/markets?ids=cardano&vs_currency=usd
type CurrencyRateGeckoResponse = Array<{
  current_price: number;
}>;
const id = 'coingecko';
const name = 'CoinGecko';
const website = 'https://www.coingecko.com/en/api';
const hostname = 'api.coingecko.com';
const version = 'v3';
const pathBase = `api/${version}`;
const requests = {
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
  rate: ({ code }: LocalizedCurrency) => ({
    hostname,
    method: 'GET',
    path: `/${pathBase}/coins/markets?ids=cardano&vs_currency=${code}`,
  }),
};
const responses = {
  list: (apiResponse: Array<Record<string, any>>): GetCurrencyListResponse => {
    try {
      if (!Array.isArray(apiResponse) || apiResponse.length < 2) {
        throw new Error('unexpected API response');
      }

      const [completeList, vsCurrencies] = apiResponse;
      const list = vsCurrencies
        .map(
          (code) =>
            currenciesList[code] ||
            completeList.find((currency) => currency.symbol === code)
        )
        .filter((item) => !!item);
      logger.debug('Currency::CoingGecko::List success', {
        list,
      });
      return list;
    } catch (error) {
      logger.error('Currency::CoingGecko::List error', {
        error,
      });
      throw new Error(error);
    }
  },
  rate: (apiResponse: CurrencyRateGeckoResponse): GetCurrencyRateResponse => {
    try {
      const rate = get(apiResponse, '[0].current_price', 0);
      logger.debug('Currency::CoingGecko::Rate success', {
        rate,
      });
      return rate;
    } catch (error) {
      logger.error('Currency::CoingGecko::Rate error', {
        error,
      });
      throw new Error(error);
    }
  },
};
// @ts-ignore ts-migrate(2352) FIXME: Conversion of type '{ id: string; name: string; ho... Remove this comment to see the full error message
export default {
  id,
  name,
  hostname,
  website,
  requests,
  responses,
} as CurrencyApiConfig;

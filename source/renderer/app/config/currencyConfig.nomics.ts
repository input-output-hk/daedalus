/**
 *
 * Nomics API
 *
 * https://nomics.com/docs/
 *
 * check `currencyConfig.js` for more info
 *
 */
import { get, values } from 'lodash';
import { logger } from '../utils/logging';
import type { Currency, CurrencyApiConfig } from '../types/currencyTypes';
import type {
  GetCurrencyListResponse,
  GetCurrencyRateResponse,
} from '../api/wallets/types';
import currenciesList from './currenciesList.json';
// For the complete response, check
// https://nomics.com/docs/#operation/getCurrenciesTicker
type CurrencyRateNomicsResponse = Array<{
  price: string;
}>;
const id = 'nomics';
const name = 'Nomics';
const website = 'https://nomics.com/docs/';
const hostname = 'api.nomics.com';
const version = 'v1';
// If we need to use NOMICS, we will need to get a valid key
const apiKey = 'API_KEY';
const requests = {
  rate: ({ code }: Currency) => ({
    hostname,
    method: 'GET',
    path: `/${version}/currencies/ticker?key=${apiKey}&ids=ADA&interval=1d,30d&&per-page=100&page=1&convert=${code.toUpperCase()}`,
  }),
};
const responses = {
  list: (): GetCurrencyListResponse => {
    try {
      const list = values(currenciesList);
      logger.debug('Currency::Nomics::List success', {
        list,
      });
      return list;
    } catch (error) {
      logger.error('Currency::Nomics::List error', {
        error,
      });
      throw new Error(error);
    }
  },
  rate: (apiResponse: CurrencyRateNomicsResponse): GetCurrencyRateResponse => {
    try {
      const rate = parseFloat(get(apiResponse, '[0].price', 0));
      logger.debug('Currency::Nomics::Rate success', {
        rate,
      });
      return rate;
    } catch (error) {
      logger.error('Currency::Nomics::Rate error', {
        error,
      });
      throw new Error(error);
    }
  },
};
export default {
  id,
  name,
  hostname,
  website,
  requests,
  responses,
} as CurrencyApiConfig;

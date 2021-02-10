// @flow

/**
 *
 * CoinAPI API
 *
 * https://www.coinapi.io/
 *
 * check `currencyConfig.js` for more info
 *
 */
import { get, values } from 'lodash';
import { logger } from '../utils/logging';
import type { Currency, CurrencyApiConfig } from '../types/currencyTypes.js';
import type {
  GetCurrencyListResponse,
  GetCurrencyRateResponse,
} from '../api/wallets/types';
import currenciesList from './currenciesList.json';

// For the complete response, check
// https://docs.coinapi.io/#get-specific-rate
type CurrencyRateCoinApiResponse = {
  rate: string,
};

const id = 'coinapi';
const name = 'CoinAPI';
const website = 'https://www.coinapi.io/';
const hostname = 'rest.coinapi.io';
const version = 'v1';
// If we need to use COINAPI, we will need to get a valid key
const apiKey = 'API_KEY';

const requests = {
  rate: ({ symbol }: Currency) => ({
    hostname,
    method: 'GET',
    path: `/${version}/exchangerate/ADA/${symbol.toUpperCase()}?apikey=${apiKey}`,
  }),
};

const responses = {
  list: (): GetCurrencyListResponse => {
    try {
      const list = values(currenciesList);
      logger.debug('Currency::CoinAPI::List success', { list });
      return list;
    } catch (error) {
      logger.error('Currency::CoinAPI::List error', { error });
      throw new Error(error);
    }
  },
  rate: (apiResponse: CurrencyRateCoinApiResponse): GetCurrencyRateResponse => {
    try {
      const rate = get(apiResponse, 'rate', 0);
      logger.debug('Currency::CoinAPI::Rate success', { rate });
      return rate;
    } catch (error) {
      logger.error('Currency::CoinAPI::Rate error', { error });
      throw new Error(error);
    }
  },
};

export default ({
  id,
  name,
  hostname,
  website,
  requests,
  responses,
}: CurrencyApiConfig);

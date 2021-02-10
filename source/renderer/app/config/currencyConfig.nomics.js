// @flow

/**
 *
 * Nomics API
 *
 * https://nomics.com/docs/
 *
 * check `currencyConfig.js` for more info
 *
 */
import { values, get } from 'lodash';
import type { Currency, CurrencyApiConfig } from '../types/currencyTypes.js';
import type { GetCurrencyRateResponse } from '../api/wallets/types';
import currenciesList from './currenciesList.json';

// For the complete response, check
// https://nomics.com/docs/#operation/getCurrenciesTicker
type CurrencyRateNomicsResponse = Array<{
  price: string,
}>;

const id = 'nomics';
const name = 'Nomics';
const website = 'https://nomics.com/docs/';
const hostname = 'api.nomics.com';
const version = 'v1';
// If we need to use NOMICS, we will need to get a valid key
const apiKey = 'demo-b5d84e505a11969a7184f899fbb40ae1';

const requests = {
  rate: ({ symbol }: Currency) => ({
    hostname,
    method: 'GET',
    path: `/${version}/currencies/ticker?key=${apiKey}&ids=ADA&interval=1d,30d&&per-page=100&page=1&convert=${symbol.toUpperCase()}`,
  }),
};

const responses = {
  list: () => values(currenciesList),
  rate: (apiResponse: CurrencyRateNomicsResponse): GetCurrencyRateResponse =>
    parseFloat(get(apiResponse, '[0].price', 0)),
};

export default ({
  id,
  name,
  hostname,
  website,
  requests,
  responses,
}: CurrencyApiConfig);

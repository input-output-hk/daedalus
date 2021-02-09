// @flow
import type { CurrencyApiConfig } from '../types/currencyTypes.js';
// import type {
//   GetCurrencyApiStatusResponse,
//   GetCurrencyListResponse,
//   GetCurrencyRateResponse,
// } from '../api/wallets/types';

const id = 'nomics';
const name = 'Nomics';
const hostname = 'https://api.nomics.com';
const website = 'https://nomics.com/docs/';

const requests = {
  status: {
    hostname,
    path: 'status',
  },
  list: {
    hostname,
    path: 'list',
  },
  rate: {
    hostname,
    path: 'rate',
  },
};

const responses = {
  status: () => {},
  list: () => {},
  rate: () => {},
};

export default ({
  id,
  name,
  hostname,
  website,
  requests,
  responses,
}: CurrencyApiConfig);

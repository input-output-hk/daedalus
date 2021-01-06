// @flow
import type { CurrencyApiConfig } from '../types/currencyTypes.js';
// import type {
//   GetCurrencyApiStatusResponse,
//   GetCurrencyListResponse,
//   GetCurrencyRateResponse,
// } from '../api/wallets/types';

const id = 'nomics';
const hostname = 'https://api.nomics.com';
const url = hostname;

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
  hostname,
  requests,
  responses,
}: CurrencyApiConfig);

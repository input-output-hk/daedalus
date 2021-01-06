// @flow
import type { Currency, CurrencyApiConfig } from '../types/currencyTypes.js';
import type {
  GetCurrencyApiStatusResponse,
  GetCurrencyListResponse,
  GetCurrencyRateResponse,
} from '../api/wallets/types';

type CurrencyRateGeckoResponse = Array<{
  id: string,
  symbol: string,
  name: string,
  image: string,
  current_price: number,
  market_cap: number,
  market_cap_rank: number,
  fully_diluted_valuation: Object,
  total_volume: number,
  high_24h: number,
  low_24h: number,
  price_change_24h: number,
  price_change_percentage_24h: number,
  market_cap_change_24h: number,
  market_cap_change_percentage_24h: number,
  circulating_supply: number,
  total_supply: number,
  max_supply: Object,
  ath: number,
  ath_change_percentage: number,
  ath_date: string,
  atl: number,
  atl_change_percentage: number,
  atl_date: string,
  roi: Object,
  last_updated: string,
}>;

const id = 'coingecko';
const hostname = 'api.coingecko.com';
const version = 'v3';
const pathBase = `api/${version}`;
const url = `${hostname}/${version}`;

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
  list: (list): GetCurrencyListResponse => list,
  rate: (apiResponse: CurrencyRateGeckoResponse): GetCurrencyRateResponse =>
    apiResponse[0].current_price,
};

export default ({
  id,
  hostname,
  requests,
  responses,
}: CurrencyApiConfig);

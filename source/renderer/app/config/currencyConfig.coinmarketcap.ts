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

const id = 'coinmarketcap';
const name = 'CoinMarketCap';
const website = 'https://coinmarketcap.com/';
const hostname = 'pro-api.coinmarketcap.com';
const pathBase = 'v1';

const token = 'd4f38223-e65f-46ae-8d56-92ab51a021b1';
const makeRequestOptions = (path: string) => ({
  hostname,
  method: 'GET',
  path,
  headers: {
    'X-CMC_PRO_API_KEY': token,
  },
});

const requests = {
  list: makeRequestOptions(`/${pathBase}/cryptocurrency/map`),
  rate: ({ code }: LocalizedCurrency) =>
    makeRequestOptions(
      `/${pathBase}/tools/price-conversion?symbol=ADA&amount=10&convert=${code}`
    ),
};
type ApiResponse<Data extends object> = {
  status: {
    error_code: number;
  };
  data?: Data;
};
type ListApiResponse = ApiResponse<
  Array<{
    id: number;
    name: string;
    sign: string;
    symbol: string;
  }>
>;
type ConversionApiResponse = ApiResponse<{
  quote: Record<string, { price: number }>;
}>;

const ensureCorrectResponse = <Data extends object>(
  response: ApiResponse<Data>,
  operationName: 'List' | 'Rate'
): response is ApiResponse<Data> & { data: Data } => {
  if (response.status.error_code === 0 && !!response.data) return;

  const errorMessage = 'unexpected API response';
  logger.error(`Currency::CoinMarketCap::${operationName} error`, {
    error: errorMessage,
  });
  throw new Error(errorMessage);
};

const responses = {
  list: (apiResponse: ListApiResponse): GetCurrencyListResponse => {
    ensureCorrectResponse(apiResponse, 'List');

    const supportedCodes = apiResponse.data.map(({ symbol }) =>
      symbol.toLowerCase()
    );
    const list = Object.values(currenciesList).filter(({ code }) =>
      supportedCodes.includes(code)
    );
    logger.debug('Currency::CoingGecko::List success', {
      list,
    });
    return list;
  },
  rate: (apiResponse: ConversionApiResponse): GetCurrencyRateResponse => {
    ensureCorrectResponse(apiResponse, 'Rate');

    const rate = Object.values(apiResponse.data.quote)[0].price;
    logger.debug('Currency::CoingGecko::Rate success', {
      rate,
    });
    return rate;
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

'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
/**
 *
 * CoingGecko API
 *
 * https://www.coingecko.com/en/api
 *
 * check `currencyConfig.js` for more info
 *
 */
const lodash_1 = require('lodash');
const logging_1 = require('../utils/logging');
const currenciesList_json_1 = __importDefault(require('./currenciesList.json'));
const id = 'coingecko';
const name = 'CoinGecko';
const website = 'https://www.coingecko.com/en/api';
const hostname = 'coingecko.live-mainnet.eks.lw.iog.io';
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
  rate: ({ code }) => ({
    hostname,
    method: 'GET',
    path: `/${pathBase}/coins/markets?ids=cardano&vs_currency=${code}`,
  }),
};
const responses = {
  list: (apiResponse) => {
    try {
      if (!Array.isArray(apiResponse) || apiResponse.length < 2) {
        throw new Error('unexpected API response');
      }
      const [completeList, vsCurrencies] = apiResponse;
      const list = vsCurrencies
        .map(
          (code) =>
            currenciesList_json_1.default[code] ||
            completeList.find((currency) => currency.symbol === code)
        )
        .filter((item) => !!item);
      logging_1.logger.debug('Currency::CoingGecko::List success', {
        list,
      });
      return list;
    } catch (error) {
      logging_1.logger.error('Currency::CoingGecko::List error', {
        error,
      });
      throw new Error(error);
    }
  },
  rate: (apiResponse) => {
    try {
      const rate = (0, lodash_1.get)(apiResponse, '[0].current_price', 0);
      logging_1.logger.debug('Currency::CoingGecko::Rate success', {
        rate,
      });
      return rate;
    } catch (error) {
      logging_1.logger.error('Currency::CoingGecko::Rate error', {
        error,
      });
      throw new Error(error);
    }
  },
};
// @ts-ignore ts-migrate(2352) FIXME: Conversion of type '{ id: string; name: string; ho... Remove this comment to see the full error message
exports.default = {
  id,
  name,
  hostname,
  website,
  requests,
  responses,
};
//# sourceMappingURL=currencyConfig.coingecko.js.map

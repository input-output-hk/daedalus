'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
/**
 *
 * CoinAPI API
 *
 * https://www.coinapi.io/
 *
 * check `currencyConfig.js` for more info
 *
 */
const lodash_1 = require('lodash');
const logging_1 = require('../utils/logging');
const currenciesList_json_1 = __importDefault(require('./currenciesList.json'));
const id = 'coinapi';
const name = 'CoinAPI';
const website = 'https://www.coinapi.io/';
const hostname = 'rest.coinapi.io';
const version = 'v1';
// If we need to use COINAPI, we will need to get a valid key
const apiKey = 'API_KEY';
const requests = {
  rate: ({ code }) => ({
    hostname,
    method: 'GET',
    path: `/${version}/exchangerate/ADA/${code.toUpperCase()}?apikey=${apiKey}`,
  }),
};
const responses = {
  list: () => {
    try {
      const list = (0, lodash_1.values)(currenciesList_json_1.default);
      logging_1.logger.debug('Currency::CoinAPI::List success', {
        list,
      });
      return list;
    } catch (error) {
      logging_1.logger.error('Currency::CoinAPI::List error', {
        error,
      });
      throw new Error(error);
    }
  },
  rate: (apiResponse) => {
    try {
      const rate = (0, lodash_1.get)(apiResponse, 'rate', 0);
      logging_1.logger.debug('Currency::CoinAPI::Rate success', {
        rate,
      });
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string | 0' is not assignable to type 'numbe... Remove this comment to see the full error message
      return rate;
    } catch (error) {
      logging_1.logger.error('Currency::CoinAPI::Rate error', {
        error,
      });
      throw new Error(error);
    }
  },
};
exports.default = {
  id,
  name,
  hostname,
  website,
  requests,
  responses,
};
//# sourceMappingURL=currencyConfig.coinapi.js.map

'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
/**
 *
 * Nomics API
 *
 * https://nomics.com/docs/
 *
 * check `currencyConfig.js` for more info
 *
 */
const lodash_1 = require('lodash');
const logging_1 = require('../utils/logging');
const currenciesList_json_1 = __importDefault(require('./currenciesList.json'));
const id = 'nomics';
const name = 'Nomics';
const website = 'https://nomics.com/docs/';
const hostname = 'api.nomics.com';
const version = 'v1';
// If we need to use NOMICS, we will need to get a valid key
const apiKey = 'API_KEY';
const requests = {
  rate: ({ code }) => ({
    hostname,
    method: 'GET',
    path: `/${version}/currencies/ticker?key=${apiKey}&ids=ADA&interval=1d,30d&&per-page=100&page=1&convert=${code.toUpperCase()}`,
  }),
};
const responses = {
  list: () => {
    try {
      const list = (0, lodash_1.values)(currenciesList_json_1.default);
      logging_1.logger.debug('Currency::Nomics::List success', {
        list,
      });
      return list;
    } catch (error) {
      logging_1.logger.error('Currency::Nomics::List error', {
        error,
      });
      throw new Error(error);
    }
  },
  rate: (apiResponse) => {
    try {
      const rate = parseFloat(apiResponse?.[0]?.price || '0');
      logging_1.logger.debug('Currency::Nomics::Rate success', {
        rate,
      });
      return rate;
    } catch (error) {
      logging_1.logger.error('Currency::Nomics::Rate error', {
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
//# sourceMappingURL=currencyConfig.nomics.js.map

'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getCurrencyRate = void 0;
const currencyConfig_1 = require('../../../config/currencyConfig');
const requestName = currencyConfig_1.REQUESTS.RATE;
const getCurrencyRate = (currency) =>
  (0, currencyConfig_1.genericCurrencyRequest)(requestName)(currency);
exports.getCurrencyRate = getCurrencyRate;
//# sourceMappingURL=getCurrencyRate.js.map

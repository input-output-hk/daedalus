'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getCurrencyList = void 0;
const currencyConfig_1 = require('../../../config/currencyConfig');
const requestName = currencyConfig_1.REQUESTS.LIST;
exports.getCurrencyList = (0, currencyConfig_1.genericCurrencyRequest)(
  requestName
);
//# sourceMappingURL=getCurrencyList.js.map

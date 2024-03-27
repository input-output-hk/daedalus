'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNetworkClock = void 0;
const request_1 = require('../../utils/request');
const getNetworkClock = (config, isForceCheck) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: '/v2/network/clock',
      ...config,
    },
    {
      forceNtpCheck: isForceCheck,
    }
  );
exports.getNetworkClock = getNetworkClock;
//# sourceMappingURL=getNetworkClock.js.map

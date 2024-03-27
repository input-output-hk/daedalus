'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getStakePools = void 0;
const request_1 = require('../../utils/request');
const getStakePools = (config, stake) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: '/v2/stake-pools',
      ...config,
    },
    {
      stake,
    }
  );
exports.getStakePools = getStakePools;
//# sourceMappingURL=getStakePools.js.map

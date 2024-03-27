'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.checkSmashServerHealth = void 0;
const request_1 = require('../../utils/request');
const checkSmashServerHealth = (config, url) =>
  (0, request_1.request)(
    {
      method: 'GET',
      path: '/v2/smash/health',
      ...config,
    },
    {
      url,
    }
  );
exports.checkSmashServerHealth = checkSmashServerHealth;
//# sourceMappingURL=checkSmashServerHealth.js.map

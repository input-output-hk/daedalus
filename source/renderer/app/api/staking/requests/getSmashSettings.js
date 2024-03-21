'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getSmashSettings = void 0;
const request_1 = require('../../utils/request');
const getSmashSettings = (config) =>
  (0, request_1.request)({
    method: 'GET',
    path: '/v2/settings',
    ...config,
  });
exports.getSmashSettings = getSmashSettings;
//# sourceMappingURL=getSmashSettings.js.map

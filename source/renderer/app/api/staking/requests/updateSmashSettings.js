'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.updateSmashSettings = void 0;
const request_1 = require('../../utils/request');
const updateSmashSettings = (config, poolMetadataSource) =>
  (0, request_1.request)(
    {
      method: 'PUT',
      path: '/v2/settings',
      ...config,
    },
    {},
    {
      settings: {
        pool_metadata_source: poolMetadataSource,
      },
    }
  );
exports.updateSmashSettings = updateSmashSettings;
//# sourceMappingURL=updateSmashSettings.js.map

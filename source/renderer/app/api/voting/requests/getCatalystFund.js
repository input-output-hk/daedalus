'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getCatalystFund = void 0;
const externalRequest_1 = require('../../utils/externalRequest');
const urlsConfig_1 = require('../../../config/urlsConfig');
const getCatalystFund = () => {
  const urlOverride = environment.catalystApiUrlOverride
    ? new URL(environment.catalystApiUrlOverride)
    : undefined;
  return (0, externalRequest_1.externalRequest)({
    hostname: urlOverride
      ? urlOverride.hostname
      : urlsConfig_1.CATALYST_API_URL,
    path: '/api/v0/fund',
    method: 'GET',
    port: urlOverride ? Number(urlOverride.port) : undefined,
    protocol: urlOverride ? urlOverride.protocol.replace(':', '') : 'https',
  });
};
exports.getCatalystFund = getCatalystFund;
//# sourceMappingURL=getCatalystFund.js.map

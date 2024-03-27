'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNewsHash = void 0;
const externalRequest_1 = require('../../utils/externalRequest');
const network_1 = require('../../../utils/network');
// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const hostname = (0, network_1.getNewsHashURL)(network);
const path = isFlight
  ? '/newsfeed-verification/mainnet_flight'
  : `/newsfeed-verification/${network}`;
const getNewsHash = (timestamp) =>
  (0, externalRequest_1.externalRequest)(
    {
      hostname,
      path: `${path}/${timestamp}.txt`,
      method: 'GET',
      protocol: 'https',
    },
    true
  );
exports.getNewsHash = getNewsHash;
//# sourceMappingURL=getNewsHash.js.map

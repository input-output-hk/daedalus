'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNews = void 0;
const externalRequest_1 = require('../../utils/externalRequest');
const network_1 = require('../../../utils/network');
// @ts-ignore ts-migrate(2339) FIXME: Property 'isFlight' does not exist on type 'typeof... Remove this comment to see the full error message
const { isFlight, environment } = global;
const { network } = environment;
const hostname = (0, network_1.getNewsURL)(network);
const path = '/newsfeed';
const filename = isFlight
  ? 'newsfeed_mainnet_flight.json'
  : `newsfeed_${network}.json`;
const getNews = () =>
  (0, externalRequest_1.externalRequest)(
    {
      hostname,
      path: `${path}/${filename}`,
      method: 'GET',
      protocol: 'https',
    },
    true
  );
exports.getNews = getNews;
//# sourceMappingURL=getNews.js.map

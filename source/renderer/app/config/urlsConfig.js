'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.ALLOWED_EXTERNAL_HOSTNAMES = exports.CATALYST_API_URL = exports.STAGING_NEWS_HASH_URL = exports.TESTNET_NEWS_HASH_URL = exports.MAINNET_NEWS_HASH_URL = exports.DEVELOPMENT_NEWS_HASH_URL = exports.STAGING_NEWS_URL = exports.TESTNET_NEWS_URL = exports.MAINNET_NEWS_URL = exports.DEVELOPMENT_NEWS_URL = exports.TESTNET_EXPLORER_URL = exports.STAGING_EXPLORER_URL = exports.MAINNET_EXPLORER_URL = void 0;
const currencyConfig_coingecko_1 = __importDefault(
  require('./currencyConfig.coingecko')
);
exports.MAINNET_EXPLORER_URL = 'explorer.cardano.org';
exports.STAGING_EXPLORER_URL = 'explorer.staging.cardano.org';
exports.TESTNET_EXPLORER_URL = 'explorer.cardano-testnet.iohkdev.io';
exports.DEVELOPMENT_NEWS_URL = 'newsfeed.daedalus.io';
exports.MAINNET_NEWS_URL = 'newsfeed.daedalus.io';
exports.TESTNET_NEWS_URL = 'newsfeed.daedalus.io';
exports.STAGING_NEWS_URL = 'newsfeed.daedalus.io';
exports.DEVELOPMENT_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
exports.MAINNET_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
exports.TESTNET_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
exports.STAGING_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
exports.CATALYST_API_URL = 'core.projectcatalyst.io';
exports.ALLOWED_EXTERNAL_HOSTNAMES = [
  exports.MAINNET_EXPLORER_URL,
  exports.STAGING_EXPLORER_URL,
  exports.TESTNET_EXPLORER_URL,
  exports.DEVELOPMENT_NEWS_URL,
  exports.MAINNET_NEWS_URL,
  exports.TESTNET_NEWS_URL,
  exports.STAGING_NEWS_URL,
  exports.DEVELOPMENT_NEWS_HASH_URL,
  exports.MAINNET_NEWS_HASH_URL,
  exports.TESTNET_NEWS_HASH_URL,
  exports.STAGING_NEWS_HASH_URL,
  currencyConfig_coingecko_1.default.hostname,
  environment.catalystApiUrlOverride
    ? new URL(environment.catalystApiUrlOverride).hostname
    : exports.CATALYST_API_URL,
];
//# sourceMappingURL=urlsConfig.js.map

'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getNewsHashURL = exports.getNewsURL = exports.getNetworkExplorerUrlByType = exports.getNetworkExplorerUrl = exports.getNetworkExplorerUri = void 0;
const urlsConfig_1 = require('../config/urlsConfig');
const environment_types_1 = require('../../../common/types/environment.types');
const getNetworkExplorerUri = (network) => {
  if (network === environment_types_1.MAINNET) {
    return urlsConfig_1.MAINNET_EXPLORER_URL;
  }
  if (network === environment_types_1.STAGING) {
    return urlsConfig_1.STAGING_EXPLORER_URL;
  }
  if (network === environment_types_1.TESTNET) {
    return urlsConfig_1.TESTNET_EXPLORER_URL;
  }
  return urlsConfig_1.MAINNET_EXPLORER_URL; // sets default to mainnet in case env.NETWORK is undefined
};
exports.getNetworkExplorerUri = getNetworkExplorerUri;
const getNetworkExplorerUrl = (network) => {
  const protocol =
    network === environment_types_1.MAINNET ||
    network === environment_types_1.TESTNET ||
    network === environment_types_1.DEVELOPMENT
      ? 'https://'
      : 'http://';
  const uri = (0, exports.getNetworkExplorerUri)(network);
  return `${protocol}${uri}`;
};
exports.getNetworkExplorerUrl = getNetworkExplorerUrl;
const getNetworkExplorerUrlByType = (type, param, network, currentLocale) => {
  let queryStringPrefix = '';
  let localePrefix = '';
  let typeValue = type;
  if (
    network === environment_types_1.MAINNET ||
    network === environment_types_1.TESTNET
  ) {
    localePrefix = `/${currentLocale.substr(0, 2)}`;
    if (type === 'address') {
      queryStringPrefix = '?address=';
      // @ts-ignore ts-migrate(2322) FIXME: Type '"address.html"' is not assignable to type '"... Remove this comment to see the full error message
      typeValue = 'address.html';
    }
    if (type === 'tx') {
      queryStringPrefix = '?id=';
      // @ts-ignore ts-migrate(2322) FIXME: Type '"transaction"' is not assignable to type '"t... Remove this comment to see the full error message
      typeValue = 'transaction';
    }
  }
  return `${(0, exports.getNetworkExplorerUrl)(
    network
  )}${localePrefix}/${typeValue}${queryStringPrefix}${param}`;
};
exports.getNetworkExplorerUrlByType = getNetworkExplorerUrlByType;
const getNewsURL = (network) => {
  // sets default to mainnet in case env.NETWORK is undefined
  let newsUrl = urlsConfig_1.MAINNET_NEWS_URL;
  if (network === environment_types_1.MAINNET) {
    newsUrl = urlsConfig_1.MAINNET_NEWS_URL;
  }
  if (network === environment_types_1.STAGING) {
    newsUrl = urlsConfig_1.STAGING_NEWS_URL;
  }
  if (network === environment_types_1.TESTNET) {
    newsUrl = urlsConfig_1.TESTNET_NEWS_URL;
  }
  if (network === environment_types_1.DEVELOPMENT) {
    newsUrl = urlsConfig_1.DEVELOPMENT_NEWS_URL;
  }
  return newsUrl;
};
exports.getNewsURL = getNewsURL;
const getNewsHashURL = (network) => {
  // sets default to mainnet in case env.NETWORK is undefined
  let newsUrl = urlsConfig_1.MAINNET_NEWS_HASH_URL;
  if (network === environment_types_1.MAINNET) {
    newsUrl = urlsConfig_1.MAINNET_NEWS_HASH_URL;
  }
  if (network === environment_types_1.STAGING) {
    newsUrl = urlsConfig_1.STAGING_NEWS_HASH_URL;
  }
  if (network === environment_types_1.TESTNET) {
    newsUrl = urlsConfig_1.TESTNET_NEWS_HASH_URL;
  }
  if (network === environment_types_1.DEVELOPMENT) {
    newsUrl = urlsConfig_1.DEVELOPMENT_NEWS_HASH_URL;
  }
  return newsUrl;
};
exports.getNewsHashURL = getNewsHashURL;
//# sourceMappingURL=network.js.map

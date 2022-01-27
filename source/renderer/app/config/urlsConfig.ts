import coingeckoConfig from './currencyConfig.coingecko';

export const MAINNET_EXPLORER_URL = 'explorer.cardano.org';
export const STAGING_EXPLORER_URL = 'explorer.staging.cardano.org';
export const TESTNET_EXPLORER_URL = 'explorer.cardano-testnet.iohkdev.io';
export const DEVELOPMENT_NEWS_URL = 'newsfeed.daedalus.io';
export const MAINNET_NEWS_URL = 'newsfeed.daedalus.io';
export const TESTNET_NEWS_URL = 'newsfeed.daedalus.io';
export const STAGING_NEWS_URL = 'newsfeed.daedalus.io';
export const DEVELOPMENT_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
export const MAINNET_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
export const TESTNET_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
export const STAGING_NEWS_HASH_URL = 'newsfeed.daedaluswallet.io';
export const ALLOWED_EXTERNAL_HOSTNAMES = [
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_NEWS_URL,
  MAINNET_NEWS_URL,
  TESTNET_NEWS_URL,
  STAGING_NEWS_URL,
  DEVELOPMENT_NEWS_HASH_URL,
  MAINNET_NEWS_HASH_URL,
  TESTNET_NEWS_HASH_URL,
  STAGING_NEWS_HASH_URL,
  coingeckoConfig.hostname,
];

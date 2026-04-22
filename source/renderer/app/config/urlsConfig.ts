import coingeckoConfig from './currencyConfig.coingecko';

export const MAINNET_EXPLORER_URL = 'explorer.cardano.org';
export const STAGING_EXPLORER_URL = 'explorer.staging.cardano.org';
export const TESTNET_EXPLORER_URL = 'explorer.cardano-testnet.iohkdev.io';

// Build-time configurable newsfeed URLs. Override NEWS_URL / NEWS_HASH_URL at
// build time (via webpack EnvironmentPlugin defaults in webpack.config.js) to
// point installers at a test server. Supports http:// and https:// schemes.
const _newsUrl = new URL(process.env.NEWS_URL || 'https://newsfeed.daedalus.io');
const _newsHashUrl = new URL(
  process.env.NEWS_HASH_URL || 'https://newsfeed.daedaluswallet.io'
);

export const NEWS_HOSTNAME = _newsUrl.hostname;
export const NEWS_PROTOCOL = _newsUrl.protocol.replace(':', '');
export const NEWS_PORT = _newsUrl.port ? parseInt(_newsUrl.port, 10) : undefined;
// Pathname prefix from NEWS_URL (e.g. '/daedalus/7.4'); empty string for root URLs.
export const NEWS_PATH_PREFIX = _newsUrl.pathname.replace(/\/$/, '');

export const NEWS_HASH_HOSTNAME = _newsHashUrl.hostname;
export const NEWS_HASH_PROTOCOL = _newsHashUrl.protocol.replace(':', '');
export const NEWS_HASH_PORT = _newsHashUrl.port
  ? parseInt(_newsHashUrl.port, 10)
  : undefined;
// Pathname prefix from NEWS_HASH_URL; empty string for root URLs.
export const NEWS_HASH_PATH_PREFIX = _newsHashUrl.pathname.replace(/\/$/, '');

// Keep per-network aliases so network.ts does not need changes.
export const DEVELOPMENT_NEWS_URL = NEWS_HOSTNAME;
export const MAINNET_NEWS_URL = NEWS_HOSTNAME;
export const TESTNET_NEWS_URL = NEWS_HOSTNAME;
export const STAGING_NEWS_URL = NEWS_HOSTNAME;
export const DEVELOPMENT_NEWS_HASH_URL = NEWS_HASH_HOSTNAME;
export const MAINNET_NEWS_HASH_URL = NEWS_HASH_HOSTNAME;
export const TESTNET_NEWS_HASH_URL = NEWS_HASH_HOSTNAME;
export const STAGING_NEWS_HASH_URL = NEWS_HASH_HOSTNAME;

export const CATALYST_API_URL = 'core.projectcatalyst.io';
export const ALLOWED_EXTERNAL_HOSTNAMES = [
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  NEWS_HOSTNAME,
  NEWS_HASH_HOSTNAME,
  coingeckoConfig.hostname,
  environment.catalystApiUrlOverride
    ? new URL(environment.catalystApiUrlOverride).hostname
    : CATALYST_API_URL,
];

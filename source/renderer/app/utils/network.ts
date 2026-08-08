import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  MAINNET_NEWS_URL,
  TESTNET_NEWS_URL,
  STAGING_NEWS_URL,
  DEVELOPMENT_NEWS_URL,
  MAINNET_NEWS_HASH_URL,
  STAGING_NEWS_HASH_URL,
  TESTNET_NEWS_HASH_URL,
  DEVELOPMENT_NEWS_HASH_URL,
} from '../config/urlsConfig';
import {
  MAINNET,
  STAGING,
  TESTNET,
  DEVELOPMENT,
  PREPROD,
  PREVIEW,
} from '../../../common/types/environment.types';

// Networks served by the current explorer.cardano.org layout, which expects
// `{root}/{network}/{tx|address}/{identifier}`.
const EXPLORER_NETWORK_PATHS: Record<string, string> = {
  [MAINNET]: 'mainnet',
  [PREPROD]: 'preprod',
  [PREVIEW]: 'preview',
};

export const getNetworkExplorerUri = (network: string): string => {
  if (network === MAINNET) {
    return MAINNET_EXPLORER_URL;
  }

  if (network === STAGING) {
    return STAGING_EXPLORER_URL;
  }

  if (network === TESTNET) {
    return TESTNET_EXPLORER_URL;
  }

  return MAINNET_EXPLORER_URL; // sets default to mainnet in case env.NETWORK is undefined
};
export const getNetworkExplorerUrl = (network: string): string => {
  const uri = getNetworkExplorerUri(network);
  // Only the staging explorer is served over plain HTTP. Every other network
  // resolves to a host that is HTTPS-only, including the mainnet explorer used
  // as the fallback for unrecognised networks.
  const protocol = uri === STAGING_EXPLORER_URL ? 'http://' : 'https://';
  return `${protocol}${uri}`;
};
export const getNetworkExplorerUrlByType = (
  type: 'tx' | 'address',
  param: string,
  network: string,
  currentLocale: string
): string => {
  const baseUrl = getNetworkExplorerUrl(network);

  // Legacy explorer host, still using locale-prefixed paths and query strings.
  if (network === TESTNET) {
    const localePrefix = `/${currentLocale.substr(0, 2)}`;
    const typeValue = type === 'address' ? 'address.html' : 'transaction';
    const queryStringPrefix = type === 'address' ? '?address=' : '?id=';
    return `${baseUrl}${localePrefix}/${typeValue}${queryStringPrefix}${param}`;
  }

  if (network === STAGING) {
    return `${baseUrl}/${type}/${param}`;
  }

  // Unknown networks fall back to the mainnet explorer host, so they also need
  // a network discriminator in the path.
  const networkPath = EXPLORER_NETWORK_PATHS[network] || 'mainnet';
  return `${baseUrl}/${networkPath}/${type}/${param}`;
};
export const getNewsURL = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let newsUrl = MAINNET_NEWS_URL;

  if (network === MAINNET) {
    newsUrl = MAINNET_NEWS_URL;
  }

  if (network === STAGING) {
    newsUrl = STAGING_NEWS_URL;
  }

  if (network === TESTNET) {
    newsUrl = TESTNET_NEWS_URL;
  }

  if (network === DEVELOPMENT) {
    newsUrl = DEVELOPMENT_NEWS_URL;
  }

  return newsUrl;
};
export const getNewsHashURL = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let newsUrl = MAINNET_NEWS_HASH_URL;

  if (network === MAINNET) {
    newsUrl = MAINNET_NEWS_HASH_URL;
  }

  if (network === STAGING) {
    newsUrl = STAGING_NEWS_HASH_URL;
  }

  if (network === TESTNET) {
    newsUrl = TESTNET_NEWS_HASH_URL;
  }

  if (network === DEVELOPMENT) {
    newsUrl = DEVELOPMENT_NEWS_HASH_URL;
  }

  return newsUrl;
};

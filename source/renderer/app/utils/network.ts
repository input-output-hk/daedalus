import {
  MAINNET_EXPLORER_URL,
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

// Networks served by explorer.cardano.org, which expects
// `{root}/{network}/{tx|address}/{identifier}`.
// Networks that are not listed here (staging, development, selfnode, the
// retired testnet, or an undefined `env.NETWORK`) have no explorer of their
// own and fall back to mainnet.
const EXPLORER_NETWORK_PATHS: Record<string, string> = {
  [MAINNET]: 'mainnet',
  [PREPROD]: 'preprod',
  [PREVIEW]: 'preview',
};
export const DEFAULT_EXPLORER_NETWORK_PATH = EXPLORER_NETWORK_PATHS[MAINNET];

// Every network is served by the same explorer host: the legacy per-network
// explorers (explorer.cardano-testnet.iohkdev.io and
// explorer.staging.cardano.org) no longer resolve.
export const getNetworkExplorerUri = (): string => MAINNET_EXPLORER_URL;
export const getNetworkExplorerUrl = (): string =>
  `https://${getNetworkExplorerUri()}`;
export const getNetworkExplorerPath = (network?: string): string =>
  EXPLORER_NETWORK_PATHS[network] || DEFAULT_EXPLORER_NETWORK_PATH;
export const getNetworkExplorerUrlByType = (
  type: 'tx' | 'address',
  param: string,
  network?: string
): string =>
  `${getNetworkExplorerUrl()}/${getNetworkExplorerPath(
    network
  )}/${type}/${param}`;
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

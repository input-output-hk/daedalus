// @flow
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
} from '../../../common/types/environment.types';

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
  const protocol =
    network === MAINNET || network === TESTNET || network === DEVELOPMENT
      ? 'https://'
      : 'http://';
  const uri = getNetworkExplorerUri(network);
  return `${protocol}${uri}`;
};

export const getNetworkExplorerUrlByType = (
  type: 'tx' | 'address',
  param: string,
  network: string,
  currentLocale: string
): string => {
  let queryStringPrefix = '';
  let localePrefix = '';
  let typeValue = type;

  if (network === MAINNET || network === TESTNET) {
    localePrefix = `/${currentLocale.substr(0, 2)}`;
    if (type === 'address') {
      queryStringPrefix = '?address=';
      typeValue = 'address.html';
    }
    if (type === 'tx') {
      queryStringPrefix = '?id=';
      typeValue = 'transaction';
    }
  }

  return `${getNetworkExplorerUrl(
    network
  )}${localePrefix}/${typeValue}${queryStringPrefix}${param}`;
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

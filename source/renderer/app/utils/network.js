// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  ITN_EXPLORER_URL,
  ITN_QA_EXPLORER_URL,
  ITN_NIGHTLY_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
  MAINNET_LATEST_VERSION_INFO_URL,
  STAGING_LATEST_VERSION_INFO_URL,
  TESTNET_LATEST_VERSION_INFO_URL,
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
  ITN_REWARDS_V1,
} from '../../../common/types/environment.types';
import {
  checkIsIncentivizedTestnetQA,
  checkIsIncentivizedTestnetNightly,
} from '../../../common/utils/environmentCheckers';

export const getNetworkExplorerUri = (
  network: string,
  rawNetwork: string
): string => {
  if (network === MAINNET) {
    return MAINNET_EXPLORER_URL;
  }
  if (network === STAGING) {
    return STAGING_EXPLORER_URL;
  }
  if (network === TESTNET) {
    return TESTNET_EXPLORER_URL;
  }
  if (checkIsIncentivizedTestnetQA(rawNetwork)) {
    return ITN_QA_EXPLORER_URL;
  }
  if (checkIsIncentivizedTestnetNightly(rawNetwork)) {
    return ITN_NIGHTLY_EXPLORER_URL;
  }
  if (network === ITN_REWARDS_V1) {
    return ITN_EXPLORER_URL;
  }
  return MAINNET_EXPLORER_URL; // sets default to mainnet incase env.NETWORK is undefined
};

export const getNetworkExplorerUrl = (
  network: string,
  rawNetwork: string
): string => {
  const protocol =
    network === MAINNET || network === DEVELOPMENT || network === ITN_REWARDS_V1
      ? 'https://'
      : 'http://';
  const uri = getNetworkExplorerUri(network, rawNetwork);
  return `${protocol}${uri}`;
};

export const getNetworkEkgUrl = (env: {
  isDev: boolean,
  isStaging: boolean,
  isTestnet: boolean,
}) => {
  // sets default to development in case env.NETWORK is undefined
  let ekgUrl = DEVELOPMENT_EKG_URL;
  if (env.isDev) {
    ekgUrl = DEVELOPMENT_EKG_URL;
  }
  if (env.isStaging) {
    ekgUrl = STAGING_EKG_URL;
  }
  if (env.isTestnet) {
    ekgUrl = TESTNET_EKG_URL;
  }
  return ekgUrl;
};

export const getLatestVersionInfoUrl = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let latestVersionInfoUrl = MAINNET_LATEST_VERSION_INFO_URL;
  if (network === MAINNET) {
    latestVersionInfoUrl = MAINNET_LATEST_VERSION_INFO_URL;
  }
  if (network === STAGING) {
    latestVersionInfoUrl = STAGING_LATEST_VERSION_INFO_URL;
  }
  if (network === TESTNET) {
    latestVersionInfoUrl = TESTNET_LATEST_VERSION_INFO_URL;
  }
  return latestVersionInfoUrl;
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

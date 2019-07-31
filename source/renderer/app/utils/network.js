// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
  MAINNET_LATEST_VERSION_INFO_URL,
  STAGING_LATEST_VERSION_INFO_URL,
  TESTNET_LATEST_VERSION_INFO_URL,
} from '../config/urlsConfig';
import {
  MAINNET,
  STAGING,
  TESTNET,
  DEVELOPMENT,
} from '../../../common/types/environment.types';

export const getNetworkExplorerUri = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (network === MAINNET) {
    explorerUrl = MAINNET_EXPLORER_URL;
  }
  if (network === STAGING) {
    explorerUrl = STAGING_EXPLORER_URL;
  }
  if (network === TESTNET) {
    explorerUrl = TESTNET_EXPLORER_URL;
  }
  return explorerUrl; // sets default to mainnet incase env.NETWORK is undefined
};

export const getNetworkExplorerUrl = (network: string): string => {
  const protocol =
    network === MAINNET || network === DEVELOPMENT ? 'https://' : 'http://';
  const uri = getNetworkExplorerUri(network);
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

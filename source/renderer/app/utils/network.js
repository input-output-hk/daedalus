// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
} from '../config/urlsConfig';
import environment from '../../../common/environment';

const { isMainnet, isStaging, isTestnet, isDevelopment } = environment;

export const getNetworkExplorerUrl = () => {
  // sets default to mainnet in case env.NETWORK is undefined
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (isMainnet()) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (isStaging()) { explorerUrl = STAGING_EXPLORER_URL; }
  if (isTestnet()) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl;
};

export const getNetworkEkgUrl = () => {
  // sets default to development in case env.NETWORK is undefined
  let ekgUrl = DEVELOPMENT_EKG_URL;
  if (isDevelopment()) { ekgUrl = DEVELOPMENT_EKG_URL; }
  if (isStaging()) { ekgUrl = STAGING_EKG_URL; }
  if (isTestnet()) { ekgUrl = TESTNET_EKG_URL; }
  return ekgUrl;
};

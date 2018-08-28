// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL
} from '../../config/urlsConfig';
import environment from '../../../../common/environment';

const { isMainnet, isStaging, isTestnet } = environment;

export const getNetworkExplorerUrl = () => {
  // sets default to mainnet incase env.NETWORK is undefined
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (isMainnet()) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (isStaging()) { explorerUrl = STAGING_EXPLORER_URL; }
  if (isTestnet()) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl;
};

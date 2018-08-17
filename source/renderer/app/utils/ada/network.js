// @flow
import {
  MAIN_NET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TEST_NET_EXPLORER_URL
} from '../../config/urlsConfig';
import environment from '../../../../common/environment';

const { isMainnet, isStaging, isTestnet } = environment;

export const getNetworkExplorerUrl = () => {
  // sets default to mainnet incase env.NETWORK is undefined
  let explorerUrl = MAIN_NET_EXPLORER_URL;
  if (isMainnet()) { explorerUrl = MAIN_NET_EXPLORER_URL; }
  if (isStaging()) { explorerUrl = STAGING_EXPLORER_URL; }
  if (isTestnet()) { explorerUrl = TEST_NET_EXPLORER_URL; }
  return explorerUrl;
};

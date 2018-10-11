// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL
} from '../config/urlsConfig';

export const getNetworkExplorerUrl = (network: string) => {
  const isMainnet = (network === 'mainnet');
  const isStaging = (network === 'staging');
  const isTestnet = (network === 'testnet');

  let explorerUrl = MAINNET_EXPLORER_URL;
  if (isMainnet) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (isStaging) { explorerUrl = STAGING_EXPLORER_URL; }
  if (isTestnet) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl; // sets default to mainnet incase env.NETWORK is undefined
};

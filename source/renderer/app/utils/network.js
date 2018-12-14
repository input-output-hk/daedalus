// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
} from '../config/urlsConfig';
import { MAINNET, STAGING, TESTNET } from '../../../common/types/environment.types';

export const getNetworkExplorerUrl = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (network === MAINNET) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (network === STAGING) { explorerUrl = STAGING_EXPLORER_URL; }
  if (network === TESTNET) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl; // sets default to mainnet incase env.NETWORK is undefined
};

export const getNetworkEkgUrl = (env: {
  isDev: boolean,
  isStaging: boolean,
  isTestnet: boolean
}) => {
  // sets default to development in case env.NETWORK is undefined
  let ekgUrl = DEVELOPMENT_EKG_URL;
  if (env.isDev) { ekgUrl = DEVELOPMENT_EKG_URL; }
  if (env.isStaging) { ekgUrl = STAGING_EKG_URL; }
  if (env.isTestnet) { ekgUrl = TESTNET_EKG_URL; }
  return ekgUrl;
};

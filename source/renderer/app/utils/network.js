// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL
} from '../config/urlsConfig';
import { MAINNET} from '../../../common/types/environment.types';
import { STAGING, TESTNET } from '../../../common/types/environment.types';

export const getNetworkExplorerUrl = (network: string): string => {
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (network === MAINNET) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (network === STAGING) { explorerUrl = STAGING_EXPLORER_URL; }
  if (network === TESTNET) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl; // sets default to mainnet incase env.NETWORK is undefined
};

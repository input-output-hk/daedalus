// @flow
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
} from '../config/urlsConfig';
import serialize from './serialize';
import { MAINNET, STAGING, TESTNET } from '../../../common/types/environment.types';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

const {
  isStaging, isTestnet, isDevelopment,
  version, os, API_VERSION, NETWORK, build, buildNumber, installerVersion
} = global.environment;

export const getNetworkExplorerUrl = (network: string): string => {
  // sets default to mainnet in case env.NETWORK is undefined
  let explorerUrl = MAINNET_EXPLORER_URL;
  if (network === MAINNET) { explorerUrl = MAINNET_EXPLORER_URL; }
  if (network === STAGING) { explorerUrl = STAGING_EXPLORER_URL; }
  if (network === TESTNET) { explorerUrl = TESTNET_EXPLORER_URL; }
  return explorerUrl; // sets default to mainnet incase env.NETWORK is undefined
};

export const getNetworkEkgUrl = () => {
  // sets default to development in case env.NETWORK is undefined
  let ekgUrl = DEVELOPMENT_EKG_URL;
  if (isDevelopment) { ekgUrl = DEVELOPMENT_EKG_URL; }
  if (isStaging) { ekgUrl = STAGING_EKG_URL; }
  if (isTestnet) { ekgUrl = TESTNET_EKG_URL; }
  return ekgUrl;
};

export const getSupportUrl = async (baseUrl: string, locale: string) => {
  const network = NETWORK === 'development' ? 'staging' : NETWORK;
  const info = {
    frontendVersion: version,
    backendVersion: API_VERSION,
    network,
    build,
    installerVersion,
    os,
    locale,
    product: `Daedalus wallet - ${network}`,
    supportLanguage: localesFillForm[locale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };
  return `${baseUrl}?${serialize(info)}`;
};


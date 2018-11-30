// @flow
import systemInformation from 'systeminformation';
import {
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  DEVELOPMENT_EKG_URL,
  STAGING_EKG_URL,
  TESTNET_EKG_URL,
} from '../config/urlsConfig';
import environment from '../../../common/environment';
import serialize from './serialize';
import getOSInfo from './getOSInfo';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

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

export const getSupportUrl = async (baseUrl: string, locale: string) => {
  const {
    version, os, API_VERSION, NETWORK, build, buildNumber, getInstallerVersion
  } = environment;
  const { release } = await systemInformation.osInfo();
  const network = NETWORK === 'development' ? 'staging' : NETWORK;
  const info = {
    frontendVersion: version,
    backendVersion: API_VERSION,
    network,
    build,
    installerVersion: getInstallerVersion(),
    os,
    locale,
    product: `Daedalus wallet - ${network}`,
    operatingSystem: getOSInfo(os, release),
    supportLanguage: localesFillForm[locale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };
  return `${baseUrl}?${serialize(info)}`;
};


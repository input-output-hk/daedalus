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
import {
  START_TIME_MAINNET,
  START_TIME_STAGING,
  START_TIME_TESTNET,
  SLOT_DURATION_MAINNET,
  SLOT_DURATION_STAGING,
  SLOT_DURATION_TESTNET,
  SLOT_DURATION_DEVNET,
  EPOCH_LENGTH_BASE_MAINNET,
  EPOCH_LENGTH_BASE_STAGING,
  EPOCH_LENGTH_BASE_TESTNET,
  EPOCH_LENGTH_BASE_DEVNET,
} from '../config/epochsConfig';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

const {
  version, os, apiVersion, network: NETWORK,
  build, buildNumber, installerVersion
} = global.environment;

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

const getEpochData = (devnetStartTime: number) => {

  if (isDevelopment()) {
    return {
      startTime: devnetStartTime,
      slotDuration: SLOT_DURATION_DEVNET,
      epochLengthBase: EPOCH_LENGTH_BASE_DEVNET,
    };
  } else if (isStaging()) {
    return {
      startTime: START_TIME_STAGING,
      slotDuration: SLOT_DURATION_STAGING,
      epochLengthBase: EPOCH_LENGTH_BASE_STAGING,
    };
  } else if (isTestnet()) {
    return {
      startTime: START_TIME_TESTNET,
      slotDuration: SLOT_DURATION_TESTNET,
      epochLengthBase: EPOCH_LENGTH_BASE_TESTNET,
    };
  }
  return {
    startTime: START_TIME_MAINNET,
    slotDuration: SLOT_DURATION_MAINNET,
    epochLengthBase: EPOCH_LENGTH_BASE_MAINNET,
  };
};

export const getCurrentEpoch = (devnetStartTime: number) => {
  const { startTime, epochLengthBase, slotDuration } = getEpochData(devnetStartTime);
  const currentTimeInUTC = Math.round((new Date()).getTime() / 1000);
  const numberOfSlots = epochLengthBase * slotDuration * 10;
  return Math.round((currentTimeInUTC - startTime) / numberOfSlots);
};

export const getSupportUrl = async (baseUrl: string, locale: string) => {
  const network = NETWORK === 'development' ? 'staging' : NETWORK;
  const info = {
    frontendVersion: version,
    backendVersion: apiVersion,
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

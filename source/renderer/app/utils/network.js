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
  SLOT_DURATION_DEVELOPMENT,
  EPOCH_LENGTH_BASE_MAINNET,
  EPOCH_LENGTH_BASE_STAGING,
  EPOCH_LENGTH_BASE_TESTNET,
  EPOCH_LENGTH_BASE_DEVELOPMENT,
} from '../config/epochsConfig';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

const {
  version, os, apiVersion, network: NETWORK,
  build, buildNumber, installerVersion,
  isMainnet, isStaging, isTestnet,
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

const getEpochData = (developmentStartTime: number) => {
  if (isMainnet) {
    return {
      startTime: START_TIME_MAINNET,
      slotDuration: SLOT_DURATION_MAINNET,
      epochLengthBase: EPOCH_LENGTH_BASE_MAINNET,
    };
  }
  if (isStaging) {
    return {
      startTime: START_TIME_STAGING,
      slotDuration: SLOT_DURATION_STAGING,
      epochLengthBase: EPOCH_LENGTH_BASE_STAGING,
    };
  }
  if (isTestnet) {
    return {
      startTime: START_TIME_TESTNET,
      slotDuration: SLOT_DURATION_TESTNET,
      epochLengthBase: EPOCH_LENGTH_BASE_TESTNET,
    };
  }
  return {
    startTime: developmentStartTime,
    slotDuration: SLOT_DURATION_DEVELOPMENT,
    epochLengthBase: EPOCH_LENGTH_BASE_DEVELOPMENT,
  };
};

export const getCurrentEpoch = (developmentStartTime: number) => {
  const { startTime, epochLengthBase, slotDuration } = getEpochData(developmentStartTime);
  const currentTimeInUTC = Math.floor(Date.now() / 1000);
  const numberOfSlots = epochLengthBase * slotDuration * 10;
  return Math.floor((currentTimeInUTC - startTime) / numberOfSlots);
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

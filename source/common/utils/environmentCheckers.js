// @flow
import { upperFirst } from 'lodash';
import {
  DEVELOPMENT,
  LINUX,
  MAC_OS,
  MAINNET,
  PRODUCTION,
  STAGING,
  TEST,
  TESTNET,
  WINDOWS,
  ITN_REWARDS_V1,
  QA,
  NIGHTLY,
  ITN_SELFNODE,
  networkPrettyNames,
} from '../types/environment.types';

/* ==================================================================
=                    Static checks and generators                   =
================================================================== */

export const evaluateNetwork = (network: ?string) => {
  let currentNetwork = network || DEVELOPMENT;
  if (network === QA || network === NIGHTLY || network === ITN_SELFNODE) {
    currentNetwork = ITN_REWARDS_V1;
  }
  return currentNetwork;
};

export const getBuildLabel = (
  buildNumber: string,
  network: string,
  currentNodeEnv: string,
  version: string
) => {
  const networkLabel = checkIsMainnet(network)
    ? 'Flight'
    : networkPrettyNames[network];
  let buildLabel = `Daedalus ${networkLabel} (${version}#${buildNumber})`;
  if (!checkIsProduction(currentNodeEnv))
    buildLabel += ` ${upperFirst(currentNodeEnv)}`;
  return buildLabel;
};

export const checkIsDev = (currentNodeEnv: string) =>
  currentNodeEnv === DEVELOPMENT;
export const checkIsTest = (currentNodeEnv: string) => currentNodeEnv === TEST;
export const checkIsProduction = (currentNodeEnv: string) =>
  currentNodeEnv === PRODUCTION;
export const checkIsMainnet = (network: string) => network === MAINNET;
export const checkIsStaging = (network: string) => network === STAGING;
export const checkIsTestnet = (network: string) => network === TESTNET;
export const checkIsIncentivizedTestnet = (network: string) =>
  network === ITN_REWARDS_V1;
export const checkIsIncentivizedTestnetQA = (rawNetwork: string) =>
  rawNetwork === QA;
export const checkIsIncentivizedTestnetNightly = (rawNetwork: string) =>
  rawNetwork === NIGHTLY;
export const checkIsIncentivizedTestnetSelfnode = (rawNetwork: string) =>
  rawNetwork === ITN_SELFNODE;
export const checkIsDevelopment = (network: string) => network === DEVELOPMENT;
export const checkIsMacOS = (platform: string) => platform === MAC_OS;
export const checkIsWindows = (platform: string) => platform === WINDOWS;
export const checkIsLinux = (platform: string) => platform === LINUX;

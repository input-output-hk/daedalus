import { upperFirst } from 'lodash';
import {
  ALONZO_PURPLE,
  SHELLEY_QA,
  DEVELOPMENT,
  LINUX,
  MAC_OS,
  MAINNET,
  MAINNET_FLIGHT,
  PRODUCTION,
  SELFNODE,
  STAGING,
  TEST,
  TESTNET,
  WINDOWS,
  networkPrettyNames,
} from '../types/environment.types';

/* ==================================================================
=                    Static checks and generators                   =
================================================================== */
export const evaluateNetwork = (network: string | null | undefined) => {
  let currentNetwork = network || DEVELOPMENT;

  if (network === MAINNET_FLIGHT) {
    currentNetwork = MAINNET;
  }

  if (network === 'alonzo-purple') {
    currentNetwork = ALONZO_PURPLE;
  }

  return currentNetwork;
};
export const getBuildLabel = (
  build: string,
  network: string,
  currentNodeEnv: string,
  isFlight: boolean,
  version: string
) => {
  const networkLabel = isFlight ? 'Flight' : networkPrettyNames[network];
  let buildLabel = `Daedalus ${networkLabel} (${version}#${build})`;
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
export const checkIsTestnet = (network: string) => network === TESTNET;
export const checkIsAlonzoPurple = (network: string) =>
  network === ALONZO_PURPLE;
export const checkIsShelleyQA = (network: string) => network === SHELLEY_QA;
export const checkIsStaging = (network: string) => network === STAGING;
export const checkIsSelfnode = (network: string) => network === SELFNODE;
export const checkIsDevelopment = (network: string) => network === DEVELOPMENT;
export const checkIsMacOS = (platform: string) => platform === MAC_OS;
export const checkIsWindows = (platform: string) => platform === WINDOWS;
export const checkIsLinux = (platform: string) => platform === LINUX;

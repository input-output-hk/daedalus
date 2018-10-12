// @flow
export type Environment = {
  NETWORK: string,
  API_VERSION: string,
  MOBX_DEV_TOOLS: boolean | string,
  current: string,
  REPORT_URL: string,
  isDev: boolean,
  isTest: boolean,
  isProduction: boolean,
  isMainnet: boolean,
  isStaging: boolean,
  isTestnet: boolean,
  build: string,
  buildNumber: string,
  buildLabel: string,
  platform: string,
  os: string,
  installerVersion: string,
  version: string,
  isWindows: boolean,
  isMacOS: boolean,
  isLinux: boolean
};

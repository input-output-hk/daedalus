// @flow
export const MAINNET_EXPLORER_URL = 'cardanoexplorer.com';
export const STAGING_EXPLORER_URL = 'cardano-explorer.awstest.iohkdev.io';
export const TESTNET_EXPLORER_URL =
  'cardano-explorer.cardano-testnet.iohkdev.io';

export const DEVELOPMENT_EKG_URL = 'http://localhost:8085';
export const STAGING_EKG_URL = 'http://localhost:8082';
export const TESTNET_EKG_URL = 'http://localhost:8081';

export const MAINNET_LATEST_VERSION_INFO_URL =
  's3-ap-northeast-1.amazonaws.com';
export const TESTNET_LATEST_VERSION_INFO_URL =
  'updates-cardano-testnet.s3.amazonaws.com';
export const STAGING_LATEST_VERSION_INFO_URL = 'update-awstest.iohkdev.io';
export const INTERNET_PING_HOSTNAME = 'ipv4.icanhazip.com';

export const ALLOWED_EXTERNAL_HOSTNAMES = [
  MAINNET_EXPLORER_URL,
  STAGING_EXPLORER_URL,
  TESTNET_EXPLORER_URL,
  MAINNET_LATEST_VERSION_INFO_URL,
  TESTNET_LATEST_VERSION_INFO_URL,
  STAGING_LATEST_VERSION_INFO_URL,
  INTERNET_PING_HOSTNAME,
];

// @flow
export const MAINNET_EXPLORER_URL = 'https://cardanoexplorer.com';
export const STAGING_EXPLORER_URL = 'http://cardano-explorer.awstest.iohkdev.io';
export const TESTNET_EXPLORER_URL = 'http://cardano-explorer.cardano-testnet.iohkdev.io';

export const DEVELOPMENT_EKG_URL = 'http://localhost:8083';
export const STAGING_EKG_URL = 'http://localhost:8082';
export const TESTNET_EKG_URL = 'http://localhost:8081';

export const MAINNET_LATEST_VERSION_INFO_URL = 'https://s3-ap-northeast-1.amazonaws.com/update.cardano-mainnet.iohk.io';
export const TESTNET_LATEST_VERSION_INFO_URL = 'https://updates-cardano-testnet.s3.amazonaws.com';
export const STAGING_LATEST_VERSION_INFO_URL = 'https://update-awstest.iohkdev.io';

export const ALLOWED_EXTERNAL_SOURCES = [
  MAINNET_LATEST_VERSION_INFO_URL,
  TESTNET_LATEST_VERSION_INFO_URL,
  STAGING_LATEST_VERSION_INFO_URL,
];

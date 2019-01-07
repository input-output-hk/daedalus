// @flow
import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (isTest: boolean, network: string): Api => ({
  ada: new AdaApi(isTest, {
    hostname: 'localhost',
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new LocalStorageApi(network),
});

// @flow
import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (isTest: boolean, NETWORK: string): Api => ({
  ada: new AdaApi(isTest, {
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new LocalStorageApi(NETWORK),
});

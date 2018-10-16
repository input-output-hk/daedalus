// @flow
import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';
import environment from '../../../common/environment';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(environment.isTest(), {
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new LocalStorageApi(),
});

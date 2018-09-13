// @flow
import AdaApi from './ada/index';
import LocalStorageApi from './localStorage/index';
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

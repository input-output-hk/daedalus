import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';

export type Api = {
  ada: AdaApi;
  localStorage: LocalStorageApi;
  setFaultyNodeSettingsApi?: boolean;
};
export const setupApi = (isTest: boolean): Api => ({
  ada: new AdaApi(isTest, {
    hostname: 'localhost',
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new LocalStorageApi(),
});

import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';

export type Api = {
  ada: AdaApi;
  localStorage: LocalStorageApi;
  setFaultyNodeSettingsApi?: boolean;
};

export const setupApi = (isTest: boolean): Api => ({
  ada: new AdaApi(isTest, {
    hostname: '127.0.0.1', // “localhost” breaks under new electron, which prefers ::1 (IPv6)
    port: 8090,
    ca: Uint8Array.from([]),
    key: Uint8Array.from([]),
    cert: Uint8Array.from([]),
  }),
  localStorage: new LocalStorageApi(),
});

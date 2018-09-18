// @flow
import { remote } from 'electron';
import AdaApi from './api';
import LocalStorageApi from './utils/localStorage';
import environment from '../../../common/environment';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(environment.isTest(), {
    port: environment.WALLET_PORT,
    ca: remote.getGlobal('ca'),
    key: remote.getGlobal('clientKey'),
    cert: remote.getGlobal('clientCert'),
  }),
  localStorage: new LocalStorageApi(),
});

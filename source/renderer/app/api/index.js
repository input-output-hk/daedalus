// @flow
import { remote } from 'electron';
import AdaApi from './ada/index';
import LocalStorageApi from './localStorage/index';
import environment from '../../../common/environment';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(environment.isTest(), {
    port: environment.WALLET_PORT,
    ca: remote.getGlobal('ca'),
  }),
  localStorage: new LocalStorageApi(),
});

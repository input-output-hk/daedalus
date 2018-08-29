// @flow
import { remote } from 'electron';
import AdaApi from './ada/index';
import EtcApi from './etc/index';
import LocalStorageApi from './localStorage/index';
import environment from '../../../common/environment';

export type Api = {
  ada: AdaApi,
  etc: EtcApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(environment.isTest(), environment.WALLET_PORT, remote.getGlobal('ca')),
  etc: new EtcApi(),
  localStorage: new LocalStorageApi(),
});

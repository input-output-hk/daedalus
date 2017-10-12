// @flow
import AdaApi from './ada/index';
import EtcApi from './etc/index';
import LocalStorageApi from './localStorage/index';

export type Api = {
  ada: AdaApi,
  etc: EtcApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(),
  etc: new EtcApi(),
  localStorage: new LocalStorageApi(),
});

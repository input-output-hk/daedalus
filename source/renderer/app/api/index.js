// @flow
import AdaApi from './ada/index';
import LocalStorageApi from './localStorage/index';

export type Api = {
  ada: AdaApi,
  localStorage: LocalStorageApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(),
  localStorage: new LocalStorageApi(),
});

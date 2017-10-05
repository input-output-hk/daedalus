// @flow
import AdaApi from './ada/index';
import EtcApi from './etc/index';

export type Api = {
  ada: AdaApi,
  etc: EtcApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(),
  etc: new EtcApi(),
});

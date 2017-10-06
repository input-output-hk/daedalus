// @flow
import AdaApi from './ada/index';
import EtcApi from './etc/index';
import LocalApi from './local/index';

export type Api = {
  ada: AdaApi,
  etc: EtcApi,
  local: LocalApi,
};

export const setupApi = (): Api => ({
  ada: new AdaApi(),
  etc: new EtcApi(),
  local: new LocalApi(),
});

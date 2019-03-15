// @flow
import Action from './lib/Action';

// ======= NETWORK STATUS ACTIONS =======

export default class NetworkStatusActions {
  isSyncedAndReady: Action<any> = new Action();
  getEpochsData: Action<any> = new Action();
  getCurrentEpochFallback: Action<any> = new Action();
}

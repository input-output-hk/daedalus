// @flow
import Action from './lib/Action';

// ======= NETWORK STATUS ACTIONS =======

export default class NetworkStatusActions {
  isSyncedAndReady: Action<any> = new Action();
  stopPoller: Action<any> = new Action();
  restartPoller: Action<any> = new Action();
}

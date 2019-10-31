// @flow
import Action from './lib/Action';

// ======= NETWORK STATUS ACTIONS =======

export default class NetworkStatusActions {
  isSyncedAndReady: Action<any> = new Action();
  tlsConfigIsReady: Action<any> = new Action();
  restartNode: Action<any> = new Action();
  nodeImplementationUpdate: Action<any> = new Action();
}

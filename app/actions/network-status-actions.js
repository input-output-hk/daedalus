// @flow
import { Action } from './lib/actions';

// ======= NETWORK STATUS ACTIONS =======

export type NetworkStatusActions = {
  isSyncedAndReady: Action<any>,
};

const networkStatusActions: NetworkStatusActions = {
  isSyncedAndReady: new Action(),
};

export default networkStatusActions;

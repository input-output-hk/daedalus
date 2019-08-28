// @flow
import Action from './lib/Action';

// ======= NODE UPDATE ACTIONS =======

export default class NodeUpdateActions {
  acceptNodeUpdate: Action<any> = new Action();
  postponeNodeUpdate: Action<any> = new Action();
  getLatestAvailableAppVersion: Action<any> = new Action();
}

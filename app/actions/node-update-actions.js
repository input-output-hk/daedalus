// @flow
import { Action } from './lib/actions';

// ======= NODE UPDATE ACTIONS =======

export default class NodeUpdateActions {
  acceptNodeUpdate: Action<any> = new Action();
  postponeNodeUpdate: Action<any> = new Action();
  toggleNodeUpdateNotificationExpanded: Action<any> = new Action();
}

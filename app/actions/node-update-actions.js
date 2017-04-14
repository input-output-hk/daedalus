// @flow
import { Action } from './lib/actions';

// ======= NODE UPDATE ACTIONS =======

export type NodeUpdateActions = {
  acceptNodeUpdate: Action<any>,
  postponeNodeUpdate: Action<any>,
  toggleNodeUpdateNotificationExpanded: Action<any>,
};

const nodeUpdateActions: NodeUpdateActions = {
  acceptNodeUpdate: new Action(),
  postponeNodeUpdate: new Action(),
  toggleNodeUpdateNotificationExpanded: new Action(),
};

export default nodeUpdateActions;

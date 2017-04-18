// @flow
import { Action } from './lib/actions';

// ======= ROUTER ACTIONS =======

export default class RouterActions {
  goToRoute: Action<{ route: string, params?: ?Object }> = new Action();
}

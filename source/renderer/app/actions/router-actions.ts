// @flow
import Action from './lib/Action';

// ======= ROUTER ACTIONS =======

export default class RouterActions {
  goToRoute: Action<{ route: string, params?: ?Object }> = new Action();
}

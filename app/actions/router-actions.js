// @flow
import { Action } from './lib/actions';

// ======= ROUTER ACTIONS =======

export type RouterActions = {
  goToRoute: Action<{ route: string, params?: ?Object }>,
};

const routerActions: RouterActions = {
  goToRoute: new Action(),
};

export default routerActions;

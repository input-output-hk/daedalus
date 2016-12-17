// @flow
import { action, extendObservable } from 'mobx';
import type { appState } from './index';

export type sidebarState = {
  route: string,
  hidden: bool,
  isMaximized: bool,
  wallets: Array<Object>,
};

const defaultValues = {
  route: '/wallets',
  hidden: false,
  isMaximized: false,
};

const state = {};

export default (root: appState): sidebarState => (extendObservable(state, defaultValues, {
  get wallets() {
    return root.user.wallets.map(wallet => ({
      id: wallet.id,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }));
  },
  reset: action(() => Object.assign(state, defaultValues))
}));

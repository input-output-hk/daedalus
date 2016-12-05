// @flow
import { action } from 'mobx';
import type { appState } from './index';

export type sidebarState = {
  route: string,
  hidden: bool,
  isMaximized: bool,
  wallets: () => Array<Object>,
};

const defaultValues = {
  route: '/wallets',
  hidden: false,
  isMaximized: false,
};

const state = {};

export default (root: appState): sidebarState => (Object.assign(state, defaultValues, {
  wallets: () => (
    root.user.wallets.map(wallet => ({
      id: wallet.address,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }))
  ),
  reset: action(() => Object.assign(state, defaultValues))
}));

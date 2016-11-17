// @flow
import type { appState } from './index';

export type sidebarState = {
  route: string,
  hidden: bool,
  showMenu: bool,
  wallets: () => Array<Object>,
};

export default (state: appState): sidebarState => ({
  route: '/wallets',
  hidden: false,
  showMenu: false,
  wallets: () => (
    state.account.wallets.map(wallet => ({
      id: wallet.address,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }))
  )
});

// @flow
import type { appState } from './index';

export type sidebarState = {
  state: appState,
  route: string,
  hidden: bool,
  showMenu: bool,
  wallets: () => Array<Object>,
};

export default (state: appState): sidebarState => ({
  state,
  route: '/wallets',
  hidden: false,
  showMenu: true,
  wallets: () => (
    state.account.userAccount.wallets.map(wallet => ({
      id: wallet.address,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }))
  )
});

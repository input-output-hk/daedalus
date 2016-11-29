// @flow
import type { appState } from './index';

export type sidebarState = {
  route: string,
  hidden: bool,
  isMaximized: bool,
  wallets: () => Array<Object>,
};

export default (state: appState): sidebarState => ({
  route: '/wallets',
  hidden: false,
  isMaximized: false,
  wallets: () => (
    state.user.wallets.map(wallet => ({
      id: wallet.address,
      title: wallet.name,
      info: `${wallet.amount} ${wallet.currency}`
    }))
  )
});

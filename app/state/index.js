// @flow
import { PropTypes } from 'react';
import { observable, extendObservable } from 'mobx';
import account from './account';
import type { accountState } from './account';
import sidebar from './sidebar';
import type { sidebarState } from './sidebar';
import activeWallet from './active-wallet';
import type { activeWalletState } from './active-wallet';

export type appState = {
  router: ?Object,
  i18n: Object,
  account: accountState,
  sidebar: sidebarState,
  activeWallet: activeWalletState
};

export default (): appState => {
  const state = observable({
    router: null,
    i18n: { locale: 'en-US' },
  });

  extendObservable(
    state,
    {
      account: account(state),
      sidebar: sidebar(state),
      activeWallet: activeWallet(state)
    }
  );

  return state;
};

export const appStatePropType = PropTypes.shape({
  account: PropTypes.object,
  router: PropTypes.object,
  i18n: PropTypes.object,
  sidebar: PropTypes.object,
  wallets: PropTypes.object
});

// @flow
import { PropTypes } from 'react';
import { observable, extendObservable } from 'mobx';
import User from '../domain/User';
import login from './login';
import type { loginState } from './login';
import settings from './settings';
import type { settingsState } from './settings';
import sidebar from './sidebar';
import type { sidebarState } from './sidebar';
import activeWallet from './active-wallet';
import type { activeWalletState } from './active-wallet';

export type appState = {
  user: User,
  router: ?Object,
  i18n: Object,
  login: loginState,
  isApplicationLoading: () => boolean,
  settings: settingsState,
  sidebar: sidebarState,
  activeWallet: activeWalletState
};

export default (): appState => {
  const state = observable({
    user: new User(),
    router: null,
    i18n: { locale: 'en-US' }
  });

  extendObservable(
    state,
    {
      get isApplicationLoading() {
        return state.activeWallet.isLoading || state.login.isLoading;
      },
      login: login(state),
      settings: settings(state),
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

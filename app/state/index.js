// @flow
import { PropTypes } from 'react';
import { observable, extendObservable, action } from 'mobx';
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
  router: {
    location: {
      hash: string,
      key: string,
      pathname: string,
      query: string,
      search: string,
      state: string
    }
  },
  i18n: {
    locale: string,
  },
  login: loginState,
  isInitialized: bool,
  isApplicationLoading: () => boolean,
  settings: settingsState,
  sidebar: sidebarState,
  activeWallet: activeWalletState,
  isCreateWalletDialogOpen: bool,
  reset: () => null
};

export default (): appState => {
  const state = observable({
    user: new User(),
    router: { location: null },
    i18n: { locale: 'en-US' },
    isInitialized: false,
    isCreateWalletDialogOpen: false
  });

  extendObservable(
    state,
    {
      login: login(state),
      settings: settings(state),
      sidebar: sidebar(state),
      activeWallet: activeWallet(state),
      get isApplicationLoading() {
        return !state.isInitialized || state.activeWallet.isLoading || state.login.isLoading;
      },
      reset: action(() => {
        state.user = new User();
        state.i18n.locale = 'en-US';
        state.isInitialized = false;
        for (const key of Object.keys(state)) {
          const subState = state[key];
          if (subState && subState.reset) subState.reset();
        }
      })
    }
  );

  return state;
};

export const appStatePropType = PropTypes.shape({
  user: PropTypes.object,
  router: PropTypes.object,
  i18n: PropTypes.object,
  sidebar: PropTypes.object,
  wallets: PropTypes.object
});

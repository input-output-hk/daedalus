// @flow
import { PropTypes } from 'react';
import { observable, extendObservable, action } from 'mobx';
import { isfunction } from 'lodash/fp';
import User from '../domain/User';
import type { loginState } from './login';
import settings from './settings';
import type { settingsState } from './settings';
import sidebar from './sidebar';
import type { sidebarState } from './sidebar';
import activeWallet from './active-wallet';
import type { activeWalletState } from './active-wallet';

export type appState = {
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
  settings: settingsState,
  sidebar: sidebarState,
  activeWallet: activeWalletState,
  isCreateWalletDialogOpen: bool,
  reset: () => null
};

const initialState = {
  router: { location: null },
  i18n: { locale: 'en-US' },
  isCreateWalletDialogOpen: false
};

export default (stores): appState => {
  const state = observable(initialState);

  extendObservable(
    state,
    {
      settings: settings(stores),
      sidebar: sidebar(stores),
      activeWallet: activeWallet(state),
      reset: action(() => {
        // Reset sub states
        for (const key of Object.keys(state)) {
          if (isfunction(state[key].reset)) state[key].reset();
        }
        // Reset root state
        for (const key of Object.keys(initialState)) {
          state[key] = initialState[key];
        }
      })
    }
  );

  return state;
};

export const appStatePropType = PropTypes.shape({
  router: PropTypes.object,
  i18n: PropTypes.object,
  sidebar: PropTypes.object,
  settings: PropTypes.object,
  wallets: PropTypes.object
});

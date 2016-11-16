// @flow
import { PropTypes } from 'react';
import { observable, extendObservable } from 'mobx';
import sidebar from './sidebar';
import type { sidebarState } from './sidebar';
import wallets from './wallets';
import type { walletsState } from './wallets';
import Account from '../domain/Account';

export type appState = {
  account: Account,
  router: ?Object,
  i18n: Object,
  sidebar: sidebarState,
  wallets: walletsState
};

export default (): appState => {
  const state = observable({
    account: new Account(),
    router: null,
    i18n: { locale: 'en-US' },
  });

  extendObservable(
    state,
    {
      sidebar: sidebar(state),
      wallets: wallets(state)
    }
  );

  return state;
};

export const appStatePropType = PropTypes.shape({
  account: PropTypes.instanceOf(Account),
  router: PropTypes.object,
  i18n: PropTypes.object,
  sidebar: PropTypes.object,
  wallets: PropTypes.object
});

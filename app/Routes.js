// @flow
import React from 'react';
import { Route } from 'react-router';

// PAGES
import Wallet from './containers/wallet/Wallet';
import StakingPage from './containers/staking/StakingPage';
import LoadingPage from './containers/LoadingPage';
import WalletSummaryPage from './containers/wallet/WalletSummaryPage';
import WalletTransactionsPage from './containers/wallet/WalletTransactionsPage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import AdaRedemptionPage from './containers/wallet/AdaRedemptionPage';
import WalletSettingsPage from './containers/wallet/WalletSettingsPage';
import NoWalletsPage from './containers/wallet/NoWalletsPage';
import LanguageSelectionPage from './containers/profile/LanguageSelectionPage';

export const ROUTES = {
  ROOT: '/',
  STAKING: '/staking',
  ADA_REDEMPTION: '/ada-redemption',
  NO_WALLETS: '/no-wallets',
  PROFILE: {
    LANGUAGE_SELECTION: '/profile/language-selection',
  },
  WALLETS: {
    ROOT: '/wallets',
    PAGE: '/wallets/:id/:page',
    SUMMARY: '/wallets/:id/summary',
    TRANSACTIONS: '/wallets/:id/transactions',
    SEND: '/wallets/:id/send',
    RECEIVE: '/wallets/:id/receive',
    SETTINGS: '/wallets/:id/settings',
  },
};

export default (
  <div>
    <Route path={ROUTES.ROOT} component={LoadingPage} />
    <Route path={ROUTES.PROFILE.LANGUAGE_SELECTION} component={LanguageSelectionPage} />
    <Route path={ROUTES.STAKING} component={StakingPage} />
    <Route path={ROUTES.ADA_REDEMPTION} component={AdaRedemptionPage} />
    <Route path={ROUTES.NO_WALLETS} component={NoWalletsPage} />
    <Route path={ROUTES.WALLETS.ROOT} component={Wallet}>
      <Route path={ROUTES.WALLETS.SUMMARY} component={WalletSummaryPage} />
      <Route path={ROUTES.WALLETS.TRANSACTIONS} component={WalletTransactionsPage} />
      <Route path={ROUTES.WALLETS.SEND} component={WalletSendPage} />
      <Route path={ROUTES.WALLETS.RECEIVE} component={WalletReceivePage} />
      <Route path={ROUTES.WALLETS.SETTINGS} component={WalletSettingsPage} />
    </Route>
  </div>
);

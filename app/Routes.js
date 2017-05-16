// @flow
import React from 'react';
import { Route, IndexRedirect } from 'react-router';

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
import Settings from './containers/settings/Settings';
import GeneralSettingsPage from './containers/settings/categories/GeneralSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';
import TermsOfUsePage from './containers/profile/TermsOfUsePage';

export const ROUTES = {
  ROOT: '/',
  STAKING: '/staking',
  ADA_REDEMPTION: '/ada-redemption',
  NO_WALLETS: '/no-wallets',
  PROFILE: {
    LANGUAGE_SELECTION: '/profile/language-selection',
    TERMS_OF_USE: '/profile/terms-of-use',
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
  SETTINGS: {
    ROOT: '/settings',
    GENERAL: '/settings/general',
    TERMS_OF_USE: '/settings/terms-of-use',
  },
};

export default (
  <div>
    <Route path={ROUTES.ROOT} component={LoadingPage} />
    <Route path={ROUTES.PROFILE.LANGUAGE_SELECTION} component={LanguageSelectionPage} />
    <Route path={ROUTES.PROFILE.TERMS_OF_USE} component={TermsOfUsePage} />
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
    <Route path="/settings" component={Settings}>
      <IndexRedirect to="general" />
      <Route path="general" component={GeneralSettingsPage} />
      <Route path="terms-of-use" component={TermsOfUseSettingsPage} />
    </Route>
  </div>
);

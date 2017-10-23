// @flow
import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import { ROUTES } from './routes-config';
import resolver from './lib/resolver';

// PAGES
import Wallet from './containers/wallet/Wallet';
import StakingPage from './containers/staking/StakingPage';
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
import SupportSettingsPage from './containers/settings/categories/SupportSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';
import TermsOfUsePage from './containers/profile/TermsOfUsePage';
import SendLogsChoicePage from './containers/profile/SendLogsChoicePage';
import DisplaySettingsPage from './containers/settings/categories/DisplaySettingsPage';

// Dynamic container loading - resolver loads file relative to '/app/' directory
const LoadingPage = resolver('containers/LoadingPage');

export const Routes = (
  <div>
    <Route path={ROUTES.ROOT} component={LoadingPage} />
    <Route path={ROUTES.PROFILE.LANGUAGE_SELECTION} component={LanguageSelectionPage} />
    <Route path={ROUTES.PROFILE.TERMS_OF_USE} component={TermsOfUsePage} />
    <Route path={ROUTES.PROFILE.SEND_LOGS} component={SendLogsChoicePage} />
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
      <Route path="support" component={SupportSettingsPage} />
      <Route path="display" component={DisplaySettingsPage} />
    </Route>
  </div>
);

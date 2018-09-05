// @flow
import React from 'react';
import { Route, IndexRedirect } from 'react-router';
import { ROUTES } from './routes-config';

// PAGES
import Root from './containers/Root';
import AdaRedemptionPage from './containers/wallet/AdaRedemptionPage';
import WalletAddPage from './containers/wallet/WalletAddPage';
import LanguageSelectionPage from './containers/profile/LanguageSelectionPage';
import Settings from './containers/settings/Settings';
import GeneralSettingsPage from './containers/settings/categories/GeneralSettingsPage';
import SupportSettingsPage from './containers/settings/categories/SupportSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';
import TermsOfUsePage from './containers/profile/TermsOfUsePage';
import DisplaySettingsPage from './containers/settings/categories/DisplaySettingsPage';
import PaperWalletCreateCertificatePage from './containers/wallet/PaperWalletCreateCertificatePage';
import Wallet from './containers/wallet/Wallet';
import WalletSummaryPage from './containers/wallet/WalletSummaryPage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import WalletTransactionsPage from './containers/wallet/WalletTransactionsPage';
import WalletSettingsPage from './containers/wallet/WalletSettingsPage';
// import StakingPage from './containers/staking/StakingPage';

export const Routes = (
  <Route path={ROUTES.ROOT} component={Root}>
    <IndexRedirect to={ROUTES.WALLETS.ROOT} />
    <Route path={ROUTES.PROFILE.LANGUAGE_SELECTION} component={LanguageSelectionPage} />
    <Route path={ROUTES.PROFILE.TERMS_OF_USE} component={TermsOfUsePage} />
    {/* <Route path={ROUTES.STAKING} component={StakingPage} /> */}
    <Route path={ROUTES.ADA_REDEMPTION} component={AdaRedemptionPage} />
    <Route path={ROUTES.WALLETS.ADD} component={WalletAddPage} />
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
    <Route
      path={ROUTES.PAPER_WALLET_CREATE_CERTIFICATE}
      component={PaperWalletCreateCertificatePage}
    />
  </Route>
);

// @flow
import React, { Component, PropTypes } from 'react';
import { Route, IndexRedirect } from 'react-router';

// PAGES
import Wallet from './containers/wallet/Wallet';
import StakingPage from './containers/staking/StakingPage';
// import LoginPage from './containers/login/LoginPage';
import LoadingPage from './containers/LoadingPage';
import WalletHomePage from './containers/wallet/WalletHomePage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import AdaRedemptionPage from './containers/wallet/AdaRedemptionPage';
import Settings from './containers/settings/Settings';
import ProfileSettingsPage from './containers/settings/categories/ProfileSettingsPage';
import TermsOfUseSettingsPage from './containers/settings/categories/TermsOfUseSettingsPage';

export default (
  <div>
    {/*<Route path="/login" component={LoginPage} />*/}
    <Route path="/loading" component={LoadingPage} />
    <Route path="/staking" component={StakingPage} />
    <Route path="/ada-redemption" component={AdaRedemptionPage} />
    <Route path="/wallets" component={Wallet}>
      <Route path=":id/home" component={WalletHomePage} />
      <Route path=":id/send" component={WalletSendPage} />
      <Route path=":id/receive" component={WalletReceivePage} />
    </Route>
    <Route path="/settings" component={Settings}>
      <IndexRedirect to="profile" />
      <Route path="profile" component={ProfileSettingsPage} />
      <Route path="termsOfUse" component={TermsOfUseSettingsPage} />
    </Route>
  </div>
);

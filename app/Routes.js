// @flow
import React, { Component, PropTypes } from 'react';
import { Route } from 'react-router';

// PAGES
import Wallet from './containers/wallet/Wallet';
import StakingPage from './containers/staking/StakingPage';
import LoadingPage from './containers/LoadingPage';
import WalletHomePage from './containers/wallet/WalletHomePage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';
import AdaRedemptionPage from './containers/wallet/AdaRedemptionPage';

export default (
  <div>
    <Route path="/" component={LoadingPage} />
    <Route path="/staking" component={StakingPage} />
    <Route path="/ada-redemption" component={AdaRedemptionPage} />
    <Route path="/wallets" component={Wallet}>
      <Route path=":id/home" component={WalletHomePage} />
      <Route path=":id/send" component={WalletSendPage} />
      <Route path=":id/receive" component={WalletReceivePage} />
    </Route>
  </div>
);

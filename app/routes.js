// @flow
import React from 'react';
import { Route, IndexRoute } from 'react-router';
import App from './App';
import WalletHomePage from './containers/wallet/WalletHomePage';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';

export default (
  <Route path="/" component={App}>
    <IndexRoute component={WalletHomePage} />
    <Route path="wallet">
      <Route path="home" component={WalletHomePage} />
      <Route path="receive" component={WalletReceivePage} />
      <Route path="send" component={WalletSendPage} />
    </Route>
  </Route>
);

// @flow
import React from 'react';
import { Route, IndexRoute } from 'react-router';
import App from './App';
import WalletSendPage from './containers/wallet/WalletSendPage';
import WalletReceivePage from './containers/wallet/WalletReceivePage';

export default (
  <Route path="/" component={App}>
    <IndexRoute component={WalletSendPage} />
    <Route path="wallet">
      <Route path="receive" component={WalletReceivePage} />
      <Route path="send" component={WalletSendPage} />
    </Route>
  </Route>
);

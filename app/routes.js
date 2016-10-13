// @flow
import React from 'react';
import { Route, IndexRoute } from 'react-router';
import App from './App';
import WalletSendPage from './containers/wallet/WalletSendPage';

export default (
  <Route path="/" component={App}>
    <IndexRoute component={WalletSendPage} />
  </Route>
);

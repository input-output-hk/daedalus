// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './LoadingPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions') @observer
export default class Root extends Component<Props> {

  render() {
    const { stores, actions, children } = this.props;
    const { networkStatus, profile, ada, app } = stores;
    const { isNetworkStatusPage } = app;
    const { isConnected, isSynced, isSystemTimeCorrect } = networkStatus;
    const wallets = stores.ada.wallets;
    const isPageThatDoesntNeedWallets = (
      profile.isSettingsPage || ada.adaRedemption.isAdaRedemptionPage
    );
    // Just render any page that doesn't require wallets to be loaded
    if (
      (isConnected && isPageThatDoesntNeedWallets) ||
      isNetworkStatusPage // Network Status page should be loaded regardless of the network status
    ) {
      return React.Children.only(children);
    }
    if (
      !isSynced ||
      !wallets.hasLoadedWallets ||
      !isSystemTimeCorrect
    ) {
      return <LoadingPage stores={stores} actions={actions} />;
    } else if (!wallets.hasAnyWallets) {
      return <WalletAddPage />;
    }
    return React.Children.only(children);
  }
}


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
    const { networkStatus, profile, adaRedemption, app, wallets } = stores;
    const { isNetworkStatusPage } = app;
    const {
      isSynced, isNodeStopping, isNodeStopped,
      isSystemTimeCorrect, isNotEnoughDiskSpace,
    } = networkStatus;
    const isPageThatDoesntNeedWallets = (
      profile.isSettingsPage ||
      (adaRedemption.isAdaRedemptionPage && wallets.hasLoadedWallets)
    );

    // Just render any page that doesn't require wallets to be loaded or node to be connected
    if (
      isNetworkStatusPage ||
      (!isNodeStopping && !isNodeStopped && isPageThatDoesntNeedWallets)
    ) {
      return React.Children.only(children);
    }

    if (
      !isSynced ||
      !wallets.hasLoadedWallets ||
      !isSystemTimeCorrect ||
      isNotEnoughDiskSpace
    ) {
      return <LoadingPage stores={stores} actions={actions} />;
    }

    if (!wallets.hasAnyWallets) {
      return <WalletAddPage />;
    }

    return React.Children.only(children);
  }
}

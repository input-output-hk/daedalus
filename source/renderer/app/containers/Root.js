// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './loading/LoadingPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Root extends Component<Props> {
  render() {
    const { stores, actions, children } = this.props;
    const { networkStatus, profile, adaRedemption, wallets, staking } = stores;
    const { isStakingPage } = staking;
    const { isProfilePage, isSettingsPage } = profile;
    const { isAdaRedemptionPage } = adaRedemption;
    const { hasLoadedWallets } = wallets;
    const {
      isSynced,
      isNodeStopping,
      isNodeStopped,
      isSystemTimeCorrect,
      isNotEnoughDiskSpace,
    } = networkStatus;

    const isPageThatDoesntNeedWallets =
      (isAdaRedemptionPage || isStakingPage || isSettingsPage) &&
      hasLoadedWallets &&
      isSynced;

    // In case node is in stopping sequence we must show the "Connecting" screen
    // with the "Stopping Cardano node..." and "Cardano node stopped" messages
    // for all the screens except of the "Network status" screen.
    const isNodeInStoppingSequence = isNodeStopping || isNodeStopped;

    // Just render any page that doesn't require wallets to be loaded or node to be connected
    if (
      (isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
      (isProfilePage && (isNotEnoughDiskSpace || !isNodeInStoppingSequence))
    ) {
      return React.Children.only(children);
    }

    if (
      !isSynced ||
      !hasLoadedWallets ||
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

// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './loading/LoadingPage';
import SplashNetworkPage from './splash/SplashNetworkPage';
import WalletImportFileDialog from '../components/wallet/wallet-import/WalletImportFileDialog';
import type { InjectedContainerProps } from '../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Root extends Component<Props> {
  render() {
    const { stores, actions, children } = this.props;
    const {
      app,
      networkStatus,
      nodeUpdate,
      profile,
      staking,
      uiDialogs,
      wallets,
    } = stores;
    const { isStakingPage } = staking;
    const { isProfilePage, isSettingsPage } = profile;
    const { showManualUpdate } = nodeUpdate;
    const { hasLoadedWallets } = wallets;
    const {
      isConnected,
      isNodeStopping,
      isNodeStopped,
      isNotEnoughDiskSpace,
      isSplashShown,
      isSystemTimeCorrect,
    } = networkStatus;
    const { isCurrentLocaleSet, areTermsOfUseAccepted } = profile;
    const isWalletImportDialogOpen = uiDialogs.isOpen(WalletImportFileDialog);
    const isPageThatDoesntNeedWallets =
      (isStakingPage || isSettingsPage) && hasLoadedWallets && isConnected;

    // In case node is in stopping sequence we must show the "Connecting" screen
    // with the "Stopping Cardano node..." and "Cardano node stopped" messages
    // for all the screens except of the "Network status" screen.
    const isNodeInStoppingSequence = isNodeStopping || isNodeStopped;

    if (
      isCurrentLocaleSet &&
      areTermsOfUseAccepted &&
      !app.environment.isTest &&
      isSplashShown
    ) {
      return <SplashNetworkPage />;
    }

    // Just render any page that doesn't require wallets to be loaded or node to be connected
    if (
      (isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
      (isProfilePage && (isNotEnoughDiskSpace || !isNodeInStoppingSequence))
    ) {
      return React.Children.only(children);
    }

    if (
      !isConnected ||
      !hasLoadedWallets ||
      isNotEnoughDiskSpace ||
      !isSystemTimeCorrect ||
      showManualUpdate
    ) {
      return <LoadingPage stores={stores} actions={actions} />;
    }

    if (!wallets.hasAnyWallets || isWalletImportDialogOpen) {
      return <WalletAddPage />;
    }

    return React.Children.only(children);
  }
}

import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './loading/LoadingPage';
import SplashNetworkPage from './splash/SplashNetworkPage';
import RedeemItnRewardsContainer from './staking/RedeemItnRewardsContainer';
import AppUpdateContainer from './appUpdate/AppUpdateContainer';
import WalletImportFileDialog from '../components/wallet/wallet-import/WalletImportFileDialog';
import type { InjectedContainerProps } from '../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
class Root extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { stores, actions, children } = this.props;
    const {
      app,
      appUpdate,
      networkStatus,
      profile,
      staking,
      voting,
      uiDialogs,
      wallets,
    } = stores;
    const { isVotingPage } = voting;
    const { isStakingPage, redeemStep } = staking;
    const { isProfilePage, isSettingsPage } = profile;
    const { displayAppUpdateOverlay } = appUpdate;
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
      (isStakingPage || isSettingsPage || isVotingPage) &&
      hasLoadedWallets &&
      isConnected;
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

    if (!isNodeInStoppingSequence && redeemStep !== null) {
      return <RedeemItnRewardsContainer />;
    }

    if (!isNodeInStoppingSequence && displayAppUpdateOverlay) {
      return <AppUpdateContainer />;
    }

    // Just render any page that doesn't require wallets to be loaded or node to be connected
    if (
      (isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
      (isProfilePage && (isNotEnoughDiskSpace || !isNodeInStoppingSequence))
    ) {
      return children;
    }

    if (
      !isConnected ||
      !hasLoadedWallets ||
      isNotEnoughDiskSpace ||
      !isSystemTimeCorrect ||
      displayAppUpdateOverlay
    ) {
      return <LoadingPage stores={stores} actions={actions} />;
    }

    if (!wallets.hasAnyWallets || isWalletImportDialogOpen) {
      return <WalletAddPage />;
    }

    return children;
  }
}

export default Root;

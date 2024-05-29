import React, { FC, useEffect, useState } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './loading/LoadingPage';
import SplashNetworkPage from './splash/SplashNetworkPage';
import RedeemItnRewardsContainer from './staking/RedeemItnRewardsContainer';
import AppUpdateContainer from './appUpdate/AppUpdateContainer';
import WalletImportFileDialog from '../components/wallet/wallet-import/WalletImportFileDialog';
import type { InjectedContainerProps } from '../types/injectedPropsType';

interface RootProps extends InjectedContainerProps {
  children: React.ReactElement;
}

const Root: FC<RootProps> = inject(
  'stores',
  'actions'
)(
  observer(({ stores, actions, children }) => {
    // States for conditional rendering
    const [currentPage, setCurrentPage] = useState<JSX.Element | null>(null);

    // Destructure stores for convenience
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

    const isNodeInStoppingSequence = isNodeStopping || isNodeStopped;

    const shouldDisplayNetworkPage =
      isCurrentLocaleSet &&
      areTermsOfUseAccepted &&
      !app.environment.isTest &&
      isSplashShown;
    const shouldDisplayRedeemItnRewards =
      !isNodeInStoppingSequence && redeemStep !== null;
    const shouldDisplayAppUpdate =
      !isNodeInStoppingSequence && displayAppUpdateOverlay;
    const shouldDisplayLoading =
      !isConnected ||
      !hasLoadedWallets ||
      isNotEnoughDiskSpace ||
      !isSystemTimeCorrect ||
      displayAppUpdateOverlay;
    const shouldDisplayAddNewWallet =
      !wallets.hasAnyWallets || isWalletImportDialogOpen;
    const shouldDisplayNoWalletNoNodeConnected =
      (isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
      (isProfilePage && (isNotEnoughDiskSpace || !isNodeInStoppingSequence));
    // Logic to determine which page to display, replicated using a useEffect hook
    useEffect(() => {
      if (shouldDisplayNetworkPage) {
        setCurrentPage(<SplashNetworkPage />);
      } else if (shouldDisplayRedeemItnRewards) {
        setCurrentPage(<RedeemItnRewardsContainer />);
      } else if (shouldDisplayAppUpdate) {
        setCurrentPage(<AppUpdateContainer />);
      } else if (shouldDisplayNoWalletNoNodeConnected) {
        setCurrentPage(children);
      } else if (shouldDisplayLoading) {
        setCurrentPage(<LoadingPage stores={stores} actions={actions} />);
      } else if (shouldDisplayAddNewWallet) {
        setCurrentPage(<WalletAddPage />);
      } else {
        setCurrentPage(children);
      }
    }, [
      shouldDisplayNetworkPage,
      shouldDisplayRedeemItnRewards,
      shouldDisplayAppUpdate,
      shouldDisplayLoading,
      shouldDisplayAddNewWallet,
      shouldDisplayNoWalletNoNodeConnected,
      children,
    ]);

    return <>{currentPage}</>;
  })
);

export default Root;

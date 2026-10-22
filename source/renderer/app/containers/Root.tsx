import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddPage from './wallet/WalletAddPage';
import LoadingPage from './loading/LoadingPage';
import SplashNetworkPage from './splash/SplashNetworkPage';
import RedeemItnRewardsContainer from './staking/RedeemItnRewardsContainer';
import AppUpdateContainer from './appUpdate/AppUpdateContainer';
import WalletImportFileDialog from '../components/wallet/wallet-import/WalletImportFileDialog';
import type { InjectedContainerProps } from '../types/injectedPropsType';
import type { LoadingPhase } from '../../../common/types/watchdog.types';

type Props = InjectedContainerProps;

// Loading phases that show a Mithril sync or a rebuild of the chain data, or
// ask for a decision about one: the damaged blockchain data screen with its
// confirmation, the Mithril progress and error views, and the choice between
// Mithril and a sync from genesis. A first run reaches that choice only
// through the location picker, which follows the setup pages.
const CHAIN_RECOVERY_PHASES: ReadonlySet<LoadingPhase> = new Set<LoadingPhase>([
  'chain-damaged',
  'mithril-syncing',
  'bootstrap-decision',
]);

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
      backend,
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
    const { isGovernancePage } = stores.governance;
    const { displayAppUpdateOverlay } = appUpdate;
    const { hasLoadedWallets } = wallets;
    const {
      isConnected,
      isNotEnoughDiskSpace,
      isSplashShown,
      isSystemTimeCorrect,
    } = networkStatus;
    const { isSetupPage } = app;
    const { isCurrentLocaleSet, areTermsOfUseAccepted } = profile;
    const isWalletImportDialogOpen = uiDialogs.isOpen(WalletImportFileDialog);
    const isPageThatDoesntNeedWallets =
      (isStakingPage || isSettingsPage || isVotingPage || isGovernancePage) &&
      hasLoadedWallets &&
      isConnected;
    // In case node is in stopping sequence we must show the "Connecting" screen
    // with the "Stopping Cardano node..." and "Cardano node stopped" messages
    // for all the screens except of the "Network status" screen.
    const isNodeInStoppingSequence = backend.loadingPhase === 'stopping';
    // Those phases replace every page, including the ones that are otherwise
    // shown without a connected node, and so does the location picker asked
    // for from the Mithril error view. The picker of a first run follows the
    // setup pages.
    const isChainRecoveryShown =
      CHAIN_RECOVERY_PHASES.has(backend.loadingPhase) ||
      (backend.loadingPhase === 'chain-storage-setup' &&
        backend.chainStoragePickerRequested);

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
      !isChainRecoveryShown &&
      ((isPageThatDoesntNeedWallets && !isNodeInStoppingSequence) ||
        (isProfilePage &&
          (isNotEnoughDiskSpace || !isNodeInStoppingSequence || isSetupPage)))
    ) {
      return children;
    }

    if (
      !isConnected ||
      backend.loadingPhase !== 'ready' ||
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

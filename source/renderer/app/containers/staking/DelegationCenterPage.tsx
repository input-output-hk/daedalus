import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import UndelegateWalletDialogContainer from '../wallet/dialogs/settings/UndelegateWalletDialogContainer';
import UndelegateWalletConfirmationDialog from '../../components/wallet/settings/UndelegateWalletConfirmationDialog';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import DelegationCenterNoWallets from '../../components/staking/delegation-center/DelegationCenterNoWallets';
import { ROUTES } from '../../routes-config';
import {
  IS_RANKING_DATA_AVAILABLE,
  MIN_DELEGATION_FUNDS,
} from '../../config/stakingConfig';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;
const STAKE_POOLS_DELEGATING_LIST = 'stakePoolsDelegatingList';
type State = {
  selectedList?: string | null | undefined;
};
const initialState = {
  selectedList: null,
};

@inject('actions', 'stores')
@observer
class DelegationCenterPage extends Component<Props, State> {
  static defaultProps = {
    stores: null,
  };
  state = { ...initialState };
  handleSetListActive = (selectedList: string) =>
    this.setState({
      selectedList,
    });
  handleDelegate = (walletId: string) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({
      dialog: DelegationSetupWizardDialog,
    });
    updateDataForActiveDialog.trigger({
      data: {
        walletId,
      },
    });
  };
  handleUndelegate = async (walletId: string) => {
    const { dialogs } = this.props.actions;
    dialogs.open.trigger({
      dialog: UndelegateWalletConfirmationDialog,
    });
    dialogs.updateDataForActiveDialog.trigger({
      data: {
        walletId,
      },
    });
  };
  handleGoToCreateWalletClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.ADD,
    });
  };

  render() {
    const { stores } = this.props;
    const { app, uiDialogs, staking, wallets, networkStatus, profile } = stores;
    const { stakePools, getStakePoolById, fetchingStakePoolsFailed } = staking;
    const {
      isSynced,
      networkTip,
      nextEpoch,
      futureEpoch,
      isEpochsInfoAvailable,
      epochLength,
    } = networkStatus;
    const { currentLocale, currentTheme } = profile;
    const { selectedList } = this.state;
    const numberOfRankedStakePools: number = stakePools.filter(
      (stakePool) =>
        IS_RANKING_DATA_AVAILABLE && stakePool.nonMyopicMemberRewards
    ).length;

    if (!wallets.allWallets.length) {
      return (
        <DelegationCenterNoWallets
          onGoToCreateWalletClick={this.handleGoToCreateWalletClick}
          minDelegationFunds={MIN_DELEGATION_FUNDS}
        />
      );
    }

    return (
      <Fragment>
        <DelegationCenter
          wallets={wallets.allWallets}
          numberOfStakePools={stakePools.length}
          numberOfRankedStakePools={numberOfRankedStakePools}
          onDelegate={this.handleDelegate}
          onUndelegate={this.handleUndelegate}
          networkTip={networkTip}
          epochLength={epochLength}
          nextEpoch={nextEpoch}
          futureEpoch={futureEpoch}
          getStakePoolById={getStakePoolById}
          isLoading={
            !isSynced || fetchingStakePoolsFailed || !stakePools.length
          }
          isEpochsInfoAvailable={isEpochsInfoAvailable}
          currentLocale={currentLocale}
          onOpenExternalLink={app.openExternalLink}
          currentTheme={currentTheme}
          listName={STAKE_POOLS_DELEGATING_LIST}
          isListActive={selectedList === STAKE_POOLS_DELEGATING_LIST}
          containerClassName="StakingWithNavigation_page"
          setListActive={this.handleSetListActive}
        />
        {uiDialogs.isOpen(UndelegateWalletConfirmationDialog) ? (
          <UndelegateWalletDialogContainer
            onExternalLinkClick={app.openExternalLink}
          />
        ) : null}
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}

export default DelegationCenterPage;

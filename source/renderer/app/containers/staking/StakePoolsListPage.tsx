import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import StakePoolsRankingLoader from '../../components/staking/stake-pools/StakePoolsRankingLoader';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
class StakePoolsListPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleDelegate = (poolId: string) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({
      dialog: DelegationSetupWizardDialog,
    });
    updateDataForActiveDialog.trigger({
      data: {
        poolId,
      },
    });
  };
  onUpdateDelegatingStake = (selectedWalletId: string, sliderValue: number) => {
    const {
      actions: { staking: stakingActions },
    } = this.props;
    stakingActions.selectDelegationWallet.trigger(selectedWalletId);
    stakingActions.updateDelegatingStake.trigger(sliderValue);
  };
  onRankStakePools = () => {
    const {
      actions: { staking: stakingActions },
    } = this.props;
    stakingActions.rankStakePools.trigger();
  };
  handleSmashSettingsClick = () => {
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.SETTINGS.STAKE_POOLS,
    });
  };

  render() {
    const {
      uiDialogs,
      staking,
      app,
      networkStatus,
      profile,
      wallets,
    } = this.props.stores;
    const { currentTheme, currentLocale } = profile;
    const { isSynced } = networkStatus;
    const {
      stakePoolsRequest,
      stakePools,
      selectedDelegationWalletId,
      stake,
      fetchingStakePoolsFailed,
      recentStakePools,
      getStakePoolById,
      smashServerUrl,
      maxDelegationFunds,
      isFetchingStakePools,
    } = staking;
    const { all } = wallets;
    const isLoading = !isSynced || fetchingStakePoolsFailed;
    const isRanking =
      !isLoading && staking.isRanking && stakePoolsRequest.isExecuting;
    return (
      <Fragment>
        <StakePools
          wallets={all}
          currentLocale={currentLocale}
          stakePoolsList={stakePools}
          stakePoolsDelegatingList={recentStakePools}
          onOpenExternalLink={app.openExternalLink}
          currentTheme={currentTheme}
          updateDelegatingStake={this.onUpdateDelegatingStake}
          rankStakePools={this.onRankStakePools}
          selectedDelegationWalletId={selectedDelegationWalletId}
          stake={stake}
          onDelegate={this.handleDelegate}
          isLoading={isLoading}
          isFetching={isFetchingStakePools}
          isRanking={isRanking}
          getStakePoolById={getStakePoolById}
          smashServerUrl={smashServerUrl}
          onSmashSettingsClick={this.handleSmashSettingsClick}
          maxDelegationFunds={maxDelegationFunds}
        />
        {isRanking && <StakePoolsRankingLoader />}
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}

export default StakePoolsListPage;

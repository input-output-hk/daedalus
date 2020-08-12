// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import StakePoolsRankingLoader from '../../components/staking/stake-pools/StakePoolsRankingLoader';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakePoolsListPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  rankTimeoutHandler = null;

  handleDelegate = (poolId: string) => {
    const { actions } = this.props;
    const { updateDataForActiveDialog } = actions.dialogs;
    actions.dialogs.open.trigger({ dialog: DelegationSetupWizardDialog });
    updateDataForActiveDialog.trigger({
      data: { poolId },
    });
  };

  onRank = (selectedWalletId: string, sliderValue: number) => {
    if (this.rankTimeoutHandler) {
      clearTimeout(this.rankTimeoutHandler);
    }
    this.rankTimeoutHandler = setTimeout(() => {
      const {
        actions: { staking: stakingActions },
      } = this.props;
      stakingActions.selectDelegationWallet.trigger(selectedWalletId);
      stakingActions.updateStake.trigger(sliderValue);
      this.rankTimeoutHandler = null;
    }, 1000);
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
    const { currentTheme } = profile;
    const { isSynced, isShelleyDataAvailable } = networkStatus;
    const {
      stakePoolsRequest,
      stakePools,
      selectedDelegationWalletId,
      stake,
      fetchingStakePoolsFailed,
      recentStakePools,
      getStakePoolById,
    } = staking;
    const { all } = wallets;
    const isLoading = !isSynced || fetchingStakePoolsFailed;
    const isRanking =
      !isLoading && staking.isRanking && stakePoolsRequest.isExecuting;

    return (
      <Fragment>
        <StakePools
          wallets={all}
          currentLocale={profile.currentLocale}
          stakePoolsList={stakePools}
          stakePoolsDelegatingList={recentStakePools}
          onOpenExternalLink={app.openExternalLink}
          currentTheme={currentTheme}
          onRank={this.onRank}
          selectedDelegationWalletId={selectedDelegationWalletId}
          stake={stake}
          isShelleyDataAvailable={isShelleyDataAvailable}
          onDelegate={this.handleDelegate}
          isLoading={isLoading}
          isRanking={isRanking}
          getStakePoolById={getStakePoolById}
        />
        {isRanking && <StakePoolsRankingLoader />}
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}

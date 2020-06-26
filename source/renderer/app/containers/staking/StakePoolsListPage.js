// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import { getNetworkExplorerUrlByType } from '../../utils/network';
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

  onRank = (sliderValue: number) => {
    if (this.rankTimeoutHandler) {
      clearTimeout(this.rankTimeoutHandler);
    }
    this.rankTimeoutHandler = setTimeout(() => {
      const {
        actions: { staking: stakingActions },
      } = this.props;
      stakingActions.updateStake.trigger(sliderValue);
      this.rankTimeoutHandler = null;
    }, 2000);
  };

  render() {
    const { uiDialogs, staking, app, profile, wallets } = this.props.stores;
    const { currentTheme, currentLocale, environment } = profile;
    const { stakePools, fetchingStakePoolsFailed, recentStakePools } = staking;
    const { all } = wallets;
    const { network, rawNetwork } = environment;
    const getPledgeAddressUrl = (pledgeAddres: string) =>
      getNetworkExplorerUrlByType(
        'address',
        pledgeAddres,
        network,
        rawNetwork,
        currentLocale
      );

    return (
      <Fragment>
        <StakePools
          wallets={all}
          stakePoolsList={stakePools}
          stakePoolsDelegatingList={recentStakePools}
          onOpenExternalLink={app.openExternalLink}
          getPledgeAddressUrl={getPledgeAddressUrl}
          currentTheme={currentTheme}
          onRank={this.onRank}
          onDelegate={this.handleDelegate}
          isLoading={fetchingStakePoolsFailed || !stakePools.length}
        />
        {uiDialogs.isOpen(DelegationSetupWizardDialog) ? (
          <DelegationSetupWizardDialogContainer />
        ) : null}
      </Fragment>
    );
  }
}

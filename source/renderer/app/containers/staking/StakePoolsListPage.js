// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import DelegationSetupWizardDialogContainer from './dialogs/DelegationSetupWizardDialogContainer';
import DelegationSetupWizardDialog from '../../components/staking/delegation-setup-wizard/DelegationSetupWizardDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

const learnMoreUrlMap = {
  'en-US': 'https://iohk.zendesk.com/hc/en-us',
  'ja-JP': 'https://iohk.zendesk.com/hc/ja',
};

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
    const { currentTheme } = profile;
    const { stakePools, fetchingStakePoolsFailed, recentStakePools } = staking;
    const { all } = wallets;
    const learnMoreUrl = learnMoreUrlMap[profile.currentLocale];

    return (
      <Fragment>
        <StakePools
          wallets={all}
          stakePoolsList={stakePools}
          stakePoolsDelegatingList={recentStakePools}
          onOpenExternalLink={app.openExternalLink}
          learnMoreUrl={learnMoreUrl}
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

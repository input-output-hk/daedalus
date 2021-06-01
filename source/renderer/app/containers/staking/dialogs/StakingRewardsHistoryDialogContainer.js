// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer, inject } from 'mobx-react';
import StakingRewardsHistoryDialog from '../../../components/staking/rewards/StakingRewardsHistoryDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsHistoryDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  componentDidMount() {
    this.fetchStakingRewards();
  }

  fetchStakingRewards() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    actions.staking.fetchRewardsHistory.trigger({
      address: reward.rewardsAddress,
    });
  }

  render() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    const { closeActiveDialog } = actions.dialogs;
    const { currentDateFormat } = stores.profile;
    const {
      isFetchingRewardsHistory,
      rewardsHistory: rewardsHistoryObject,
    } = stores.staking;
    const {
      requestRewardsHistoryCSVFile,
      setRewardsHistoryDateRange,
    } = actions.staking;
    const rewardsHistory = rewardsHistoryObject[reward.rewardsAddress] || [];
    if (!reward) return null;
    return (
      <StakingRewardsHistoryDialog
        currentDateFormat={currentDateFormat}
        isFetchingRewardsHistory={isFetchingRewardsHistory}
        onClose={closeActiveDialog.trigger}
        onExportCSV={requestRewardsHistoryCSVFile.trigger}
        onSetDateRange={setRewardsHistoryDateRange.trigger}
        reward={reward}
        rewardsHistory={toJS(rewardsHistory)}
      />
    );
  }
}

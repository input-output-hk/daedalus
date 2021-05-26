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
    const { currentDateFormat, currentLocale } = stores.profile;
    const {
      isFetchingRewardsHistory,
      rewardsHistory: rewardsHistoryObject,
    } = stores.staking;
    const rewardsHistoryList = rewardsHistoryObject[reward.rewardsAddress];
    const rewardsHistory = rewardsHistoryList
      ? toJS(rewardsHistoryList).map((r, index) => ({
          // TODO: implement date handling
          date: new Date(Date.now() - index * 1000000000),
          epoch: r.earnedIn,
          pool: stores.staking.getStakePoolById(r.stakePoolId),
          amount: r.amount,
        }))
      : [];
    if (!reward) return null;
    return (
      <StakingRewardsHistoryDialog
        reward={reward}
        rewardsHistory={rewardsHistory}
        currentDateFormat={currentDateFormat}
        currentLocale={currentLocale}
        onClose={closeActiveDialog.trigger}
        isFetchingRewardsHistory={isFetchingRewardsHistory}
      />
    );
  }
}

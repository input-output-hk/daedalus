// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingRewardsDialog from '../../../components/staking/rewards/StakingRewardsDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

// @REWARDS TODO
import { rewardsHistory as dummyData } from '../../../config/rewardsHistory.dummy';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    const { closeActiveDialog } = actions.dialogs;
    const { currentDateFormat } = stores.profile;
    const rewardsHistory = dummyData();
    if (!reward) return null;
    return (
      <StakingRewardsDialog
        reward={reward}
        rewardsHistory={rewardsHistory}
        currentDateFormat={currentDateFormat}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

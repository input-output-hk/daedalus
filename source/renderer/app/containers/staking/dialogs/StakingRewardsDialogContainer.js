// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingRewardsDialog from '../../../components/staking/rewards/StakingRewardsDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { reward } = stores.uiDialogs.dataForActiveDialog;
    const { closeActiveDialog } = actions.dialogs;
    if (!reward) return null;
    return (
      <StakingRewardsDialog
        reward={reward}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

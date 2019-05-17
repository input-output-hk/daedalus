// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingRewards from '../../components/staking/rewards/StakingRewards';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingRewardsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return <StakingRewards {...this.props} />;
  }
}

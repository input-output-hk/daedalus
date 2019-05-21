// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingStakePools from '../../components/staking/stake-pools/StakingStakePools';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingStakePoolsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stakePools, delegatingStakePools } = this.props.stores.staking;
    return (
      <StakingStakePools
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
      />
    );
  }
}

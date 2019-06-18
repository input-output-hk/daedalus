// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakePools from '../../components/staking/stake-pools/StakePools';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakePoolsListPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { staking, app, profile } = this.props.stores;
    const { currentTheme } = profile;
    const { stakePools, delegatingStakePools } = staking;
    return (
      <StakePools
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
        onOpenExternalLink={app.openExternalLink}
        currentTheme={currentTheme}
      />
    );
  }
}

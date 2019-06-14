// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakePoolsList from '../../components/staking/stake-pools/StakePoolsList';
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
      <StakePoolsList
        stakePoolsList={stakePools}
        stakePoolsDelegatingList={delegatingStakePools}
        onOpenExternalLink={app.openExternalLink}
        currentTheme={currentTheme}
      />
    );
  }
}

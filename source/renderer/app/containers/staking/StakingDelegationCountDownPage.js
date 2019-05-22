// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingDelegationCountDown from '../../components/staking/delegation-count-down/StakingDelegationCountDown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingDelegationCountDownPage extends Component<Props> {
  static defaultProps = { actions: null, stores: {} };

  render() {
    const { stores, actions } = this.props;
    const { profile = {}, staking = {} } = stores;

    return (
      <StakingDelegationCountDown
        actions={actions}
        currentLocale={profile.currentLocale}
        startDateTime={staking.startDateTime}
      />
    );
  }
}

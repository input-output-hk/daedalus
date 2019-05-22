// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingDelegationCountDown from '../../components/staking/delegation-countdown/StakingDelegationCountDown';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores')
@observer
export default class StakingDelegationCountDownPage extends Component<Props> {
  static defaultProps = { actions: null, stores: {} };

  render() {
    const { stores } = this.props;
    const { profile = {}, staking = {} } = stores;

    return (
      <StakingDelegationCountDown
        currentLocale={profile.currentLocale}
        startDateTime={staking.startDateTime}
      />
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingDelegationCenter from '../../components/staking/delegation-center/StakingDelegationCenter';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingDelegationCenterPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return <StakingDelegationCenter name="StakingDelegationCenter" />;
  }
}

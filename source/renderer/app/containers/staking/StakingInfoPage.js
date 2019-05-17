// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import StakingInfo from '../../components/staking/info/StakingInfo';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class StakingInfoPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return <StakingInfo name="StakingInfo" />;
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import DelegationCenter from '../../components/staking/delegation-center/DelegationCenter';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class DelegationCenterPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    return <DelegationCenter name="DelegationCenter" />;
  }
}

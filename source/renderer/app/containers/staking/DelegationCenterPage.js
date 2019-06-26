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
    const {
      stores: { staking },
    } = this.props;

    return (
      <DelegationCenter
        adaValue={staking.adaValue}
        percentage={staking.percentage}
        wallets={[]}
      />
    );
  }
}

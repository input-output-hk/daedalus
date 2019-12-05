// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';
import type { NextEpoch, TipInfo } from '../../../api/network/types';

type Props = {
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
  networkTip: ?TipInfo,
  nextEpoch: ?NextEpoch,
  getStakePoolById: Function,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      wallets,
      onDelegate,
      numberOfStakePools,
      networkTip,
      nextEpoch,
      getStakePoolById,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader networkTip={networkTip} nextEpoch={nextEpoch} />
        <DelegationCenterBody
          wallets={wallets}
          onDelegate={onDelegate}
          numberOfStakePools={numberOfStakePools}
          getStakePoolById={getStakePoolById}
        />
      </Fragment>
    );
  }
}

// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';
import type {
  NextEpoch,
  TipInfo,
  FutureEpoch,
} from '../../../api/network/types';

type Props = {
  wallets: Array<Wallet>,
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  networkTip: ?TipInfo,
  nextEpoch: ?NextEpoch,
  futureEpoch: ?FutureEpoch,
  getStakePoolById: Function,
  fetchingStakePoolsFailed: boolean,
  currentLocale: string,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      wallets,
      numberOfStakePools,
      onDelegate,
      onUndelegate,
      networkTip,
      nextEpoch,
      futureEpoch,
      getStakePoolById,
      fetchingStakePoolsFailed,
      currentLocale,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader
          networkTip={networkTip}
          nextEpoch={nextEpoch}
          futureEpoch={futureEpoch}
          currentLocale={currentLocale}
        />
        <DelegationCenterBody
          wallets={wallets}
          numberOfStakePools={numberOfStakePools}
          onDelegate={onDelegate}
          onUndelegate={onUndelegate}
          getStakePoolById={getStakePoolById}
          isLoading={fetchingStakePoolsFailed}
        />
      </Fragment>
    );
  }
}

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
  SlotLength,
  EpochLength,
} from '../../../api/network/types';

type Props = {
  wallets: Array<Wallet>,
  numberOfStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  networkTip: ?TipInfo,
  slotLength: ?SlotLength,
  epochLength: ?EpochLength,
  nextEpoch: ?NextEpoch,
  futureEpoch: ?FutureEpoch,
  getStakePoolById: Function,
  isLoading: boolean,
  currentLocale: string,
  isEpochsInfoAvailable: boolean,
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
      slotLength,
      epochLength,
      nextEpoch,
      futureEpoch,
      getStakePoolById,
      isLoading,
      currentLocale,
      isEpochsInfoAvailable,
    } = this.props;

    return (
      <Fragment>
        {isEpochsInfoAvailable && (
          <DelegationCenterHeader
            networkTip={networkTip}
            slotLength={slotLength}
            epochLength={epochLength}
            nextEpoch={nextEpoch}
            futureEpoch={futureEpoch}
            currentLocale={currentLocale}
          />
        )}
        <DelegationCenterBody
          wallets={wallets}
          numberOfStakePools={numberOfStakePools}
          onDelegate={onDelegate}
          onUndelegate={onUndelegate}
          getStakePoolById={getStakePoolById}
          isLoading={isLoading || !isEpochsInfoAvailable}
        />
      </Fragment>
    );
  }
}

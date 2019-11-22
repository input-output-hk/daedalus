// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';

type Props = {
  adaValue: BigNumber,
  percentage: number,
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      adaValue,
      percentage,
      wallets,
      onDelegate,
      numberOfStakePools,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader adaValue={adaValue} percentage={percentage} />
        <DelegationCenterBody
          wallets={wallets}
          onDelegate={onDelegate}
          numberOfStakePools={numberOfStakePools}
        />
      </Fragment>
    );
  }
}

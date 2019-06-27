// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import BigNumber from 'bignumber.js';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';

type Props = {
  adaValue: BigNumber,
  percentage: number,
  wallets: Array<any>,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const { adaValue, percentage, wallets } = this.props;
    return (
      <Fragment>
        <DelegationCenterHeader adaValue={adaValue} percentage={percentage} />
        <DelegationCenterBody wallets={wallets} />
      </Fragment>
    );
  }
}

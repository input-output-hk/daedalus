// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';

type Props = {
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      wallets,
      onDelegate,
      numberOfStakePools,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterBody
          wallets={wallets}
          onDelegate={onDelegate}
          numberOfStakePools={numberOfStakePools}
        />
      </Fragment>
    );
  }
}

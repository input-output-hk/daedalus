// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';

type Props = {
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
  startDateTime: string,
  redirectToStakingInfo: Function,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      wallets,
      onDelegate,
      numberOfStakePools,
      startDateTime,
      redirectToStakingInfo,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader
          redirectToStakingInfo={redirectToStakingInfo}
          startDateTime={startDateTime}
        />
        <DelegationCenterBody
          wallets={wallets}
          onDelegate={onDelegate}
          numberOfStakePools={numberOfStakePools}
        />
      </Fragment>
    );
  }
}

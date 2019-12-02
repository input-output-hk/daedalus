// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import DelegationCenterHeader from './DelegationCenterHeader';
import DelegationCenterBody from './DelegationCenterBody';
import Wallet from '../../../domains/Wallet';
import type { TipInfo } from '../../../api/network/types';

type Props = {
  wallets: Array<Wallet>,
  onDelegate: Function,
  numberOfStakePools: number,
  startDateTime: string,
  redirectToStakingInfo: Function,
  networkTip: ?TipInfo,
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
      networkTip,
    } = this.props;

    return (
      <Fragment>
        <DelegationCenterHeader
          redirectToStakingInfo={redirectToStakingInfo}
          startDateTime={startDateTime}
          networkTip={networkTip}
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

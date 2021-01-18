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
  numberOfRankedStakePools: number,
  onDelegate: Function,
  onUndelegate: Function,
  networkTip: ?TipInfo,
  epochLength: ?number,
  nextEpoch: ?NextEpoch,
  futureEpoch: ?FutureEpoch,
  getStakePoolById: Function,
  isLoading: boolean,
  currentLocale: string,
  isEpochsInfoAvailable: boolean,
  isListActive?: boolean,
  currentTheme: string,
  onOpenExternalLink: Function,
  containerClassName: string,
  setListActive?: Function,
  listName?: string,
};

@observer
export default class DelegationCenter extends Component<Props> {
  render() {
    const {
      wallets,
      numberOfStakePools,
      numberOfRankedStakePools,
      onDelegate,
      onUndelegate,
      networkTip,
      epochLength,
      nextEpoch,
      futureEpoch,
      getStakePoolById,
      isLoading,
      currentLocale,
      isEpochsInfoAvailable,
      isListActive,
      currentTheme,
      onOpenExternalLink,
      containerClassName,
      setListActive,
      listName,
    } = this.props;

    return (
      <Fragment>
        {isEpochsInfoAvailable && (
          <DelegationCenterHeader
            networkTip={networkTip}
            epochLength={epochLength}
            nextEpoch={nextEpoch}
            futureEpoch={futureEpoch}
            currentLocale={currentLocale}
          />
        )}
        <DelegationCenterBody
          wallets={wallets}
          numberOfStakePools={numberOfStakePools}
          numberOfRankedStakePools={numberOfRankedStakePools}
          onDelegate={onDelegate}
          onUndelegate={onUndelegate}
          getStakePoolById={getStakePoolById}
          nextEpoch={nextEpoch}
          futureEpoch={futureEpoch}
          isLoading={isLoading || !isEpochsInfoAvailable}
          currentTheme={currentTheme}
          isListActive={isListActive}
          onOpenExternalLink={onOpenExternalLink}
          containerClassName={containerClassName}
          setListActive={setListActive}
          listName={listName}
        />
      </Fragment>
    );
  }
}

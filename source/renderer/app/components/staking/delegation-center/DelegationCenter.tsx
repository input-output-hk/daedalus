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
  wallets: Array<Wallet>;
  numberOfStakePools: number;
  numberOfRankedStakePools: number;
  onDelegate: (...args: Array<any>) => any;
  onUndelegate: (...args: Array<any>) => any;
  networkTip: TipInfo | null | undefined;
  epochLength: number | null | undefined;
  nextEpoch: NextEpoch | null | undefined;
  futureEpoch: FutureEpoch | null | undefined;
  getStakePoolById: (...args: Array<any>) => any;
  isLoading: boolean;
  currentLocale: string;
  isEpochsInfoAvailable: boolean;
  isListActive?: boolean;
  currentTheme: string;
  onOpenExternalLink: (...args: Array<any>) => any;
  containerClassName: string;
  setListActive?: (...args: Array<any>) => any;
  listName?: string;
};

@observer
class DelegationCenter extends Component<Props> {
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

export default DelegationCenter;

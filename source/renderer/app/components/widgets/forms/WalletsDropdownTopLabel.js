// @flow
import React, { Component } from 'react';
import SVGInline from 'react-svg-inline';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownTopLabel.scss';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import LoadingSpinner from '../LoadingSpinner';
import Wallet from '../../../domains/Wallet';

export type WalletOption = {
  wallet: $Shape<Wallet>,
  getStakePoolById: Function,
  numberOfStakePools?: number,
  isSyncing?: boolean,
  syncingLabel?: string,
};

export default class WalletsDropdownTopLabel extends Component<WalletOption> {
  renderLabel = (walletName: string, isHardwareWallet: boolean) => {
    return (
      <div className={styles.walletName}>
        {walletName}
        {isHardwareWallet && (
          <SVGInline
            svg={hardwareWalletsIcon}
            className={styles.hardwareWalletsIcon}
          />
        )}
      </div>
    );
  };

  renderLabelSyncing = (
    walletName: string,
    syncingLabel: string,
    isHardwareWallet: boolean
  ) => {
    return (
      <div className={styles.walletName}>
        {walletName}
        {isHardwareWallet && (
          <SVGInline
            svg={hardwareWalletsIcon}
            className={styles.hardwareWalletsIcon}
          />
        )}
        <span className={styles.walletNameSync}> {syncingLabel}</span>
      </div>
    );
  };

  renderTicker = () => {
    const { wallet, getStakePoolById, numberOfStakePools } = this.props;
    const {
      delegatedStakePoolId,
      lastDelegatedStakePoolId,
      pendingDelegations,
    } = wallet;
    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    let currentStakePoolId = delegatedStakePoolId;
    if (hasPendingDelegations) {
      currentStakePoolId = lastDelegatedStakePoolId;
    }
    const delegatedStakePool = getStakePoolById(currentStakePoolId);
    const { ranking, ticker } = delegatedStakePool;
    if (!numberOfStakePools || !delegatedStakePool) {
      return null;
    }
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div style={{ color }} className={styles.ticker}>
        [{ticker}]
      </div>
    );
  };

  render() {
    const { isSyncing, syncingLabel, wallet } = this.props;

    const { name: walletName, isHardwareWallet } = wallet;

    // if (!delegatedStakePool || !numberOfStakePools) {
    //   return (
    //     <div className={styles.topRow}>
    //       <div className={styles.topRowTicker}>
    //         {isSyncing && syncingLabel
    //           ? this.renderLabelSyncing(
    //               walletName,
    //               syncingLabel,
    //               isHardwareWallet
    //             )
    //           : this.renderLabel(walletName, isHardwareWallet)}
    //       </div>
    //       <div className={styles.topRowSync}>
    //         {isSyncing && (
    //           <div className={styles.syncing}>
    //             <LoadingSpinner />
    //           </div>
    //         )}
    //       </div>
    //     </div>
    //   );
    // }

    const ticker = this.renderTicker();

    return (
      <div className={styles.topRow}>
        <div className={styles.topRowTicker}>
          {ticker}
          <div className={styles.walletName}>
            {walletName}
            {isHardwareWallet && (
              <SVGInline
                svg={hardwareWalletsIcon}
                className={styles.hardwareWalletsIcon}
              />
            )}
            {!ticker && isSyncing && syncingLabel && (
              <span className={styles.walletNameSync}> {syncingLabel}</span>
            )}
          </div>
        </div>
      </div>
    );
  }
}

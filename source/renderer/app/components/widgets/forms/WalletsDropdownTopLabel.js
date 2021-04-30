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
    const delegatedStakePool = currentStakePoolId
      ? getStakePoolById(currentStakePoolId)
      : null;
    if (!numberOfStakePools || !delegatedStakePool) {
      return null;
    }
    const { ranking, ticker } = delegatedStakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div style={{ color }} className={styles.ticker}>
        [{ticker}]
      </div>
    );
  };

  render() {
    const { isSyncing, syncingLabel, wallet } = this.props;
    const { name, isHardwareWallet } = wallet;
    const ticker = this.renderTicker();
    const hasSyncing = !ticker && isSyncing && syncingLabel;
    return (
      <div className={styles.topRow}>
        <div className={styles.topRowTicker}>
          {ticker}
          <div className={styles.walletName}>
            {name}
            {isHardwareWallet && (
              <SVGInline
                svg={hardwareWalletsIcon}
                className={styles.hardwareWalletsIcon}
              />
            )}
            {hasSyncing && (
              <span className={styles.walletNameSync}> {syncingLabel}</span>
            )}
          </div>
          {hasSyncing && (
            <div className={styles.topRowSync}>
              {isSyncing && (
                <div className={styles.syncing}>
                  <LoadingSpinner />
                </div>
              )}
            </div>
          )}
        </div>
      </div>
    );
  }
}

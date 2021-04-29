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

  renderTicker = (ticker: string, color: string) => (
    <div style={{ color }} className={styles.ticker}>
      [{ticker}]
    </div>
  );

  render() {
    const {
      getStakePoolById,
      isSyncing,
      numberOfStakePools,
      syncingLabel,
      wallet,
    } = this.props;

    const {
      name: walletName,
      delegatedStakePoolId,
      lastDelegatedStakePoolId,
      pendingDelegations,
      isHardwareWallet,
    } = wallet;

    const hasPendingDelegations =
      pendingDelegations && pendingDelegations.length > 0;
    let currentStakePoolId = delegatedStakePoolId;
    if (hasPendingDelegations) {
      currentStakePoolId = lastDelegatedStakePoolId;
    }
    const delegatedStakePool = getStakePoolById(currentStakePoolId);

    if (!delegatedStakePool || !numberOfStakePools) {
      return (
        <div className={styles.topRow}>
          <div className={styles.topRowTicker}>
            {isSyncing && syncingLabel
              ? this.renderLabelSyncing(
                  walletName,
                  syncingLabel,
                  isHardwareWallet
                )
              : this.renderLabel(walletName, isHardwareWallet)}
          </div>
          <div className={styles.topRowSync}>
            {isSyncing && (
              <div className={styles.syncing}>
                <LoadingSpinner />
              </div>
            )}
          </div>
        </div>
      );
    }

    const { ranking, ticker } = delegatedStakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div className={styles.topRow}>
        <div className={styles.topRowTicker}>
          {this.renderTicker(ticker, color)}
          <div className={styles.walletName}>
            {walletName}
            {isHardwareWallet && (
              <SVGInline
                svg={hardwareWalletsIcon}
                className={styles.hardwareWalletsIcon}
              />
            )}
          </div>
        </div>
      </div>
    );
  }
}

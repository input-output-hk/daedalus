// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownOption.scss';
import StakePool from '../../../domains/StakePool';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';
import LoadingSpinner from '../LoadingSpinner';

export type WalletOption = {
  delegatedStakePool?: ?StakePool,
  detail?: string,
  label: any,
  numberOfStakePools?: number,
  selected?: boolean,
  isSyncing?: boolean,
  syncingLabel?: string,
  isHardwareWallet: boolean,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  renderLabel = (label: string, isHardwareWallet: boolean) => {
    return (
      <div className={styles.label}>
        {label}
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
    label: string,
    syncingLabel: string,
    isHardwareWallet: boolean
  ) => {
    return (
      <div className={styles.label}>
        {label}
        {isHardwareWallet && (
          <SVGInline
            svg={hardwareWalletsIcon}
            className={styles.hardwareWalletsIcon}
          />
        )}
        <span className={styles.labelSync}> {syncingLabel}</span>
      </div>
    );
  };

  renderLabelAndTicker = () => {
    const {
      delegatedStakePool,
      label,
      numberOfStakePools,
      isSyncing,
      syncingLabel,
      isHardwareWallet,
    } = this.props;
    if (!delegatedStakePool || !numberOfStakePools) {
      return (
        <div className={styles.topRow}>
          <div className={styles.topRowTicker}>
            {isSyncing && syncingLabel
              ? this.renderLabelSyncing(label, syncingLabel, isHardwareWallet)
              : this.renderLabel(label, isHardwareWallet)}
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
          <div style={{ color }} className={styles.ticker}>
            [{ticker}]
          </div>
          <div className={styles.label}>
            {label}
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
  };

  render() {
    const { detail, selected } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
    });
    return (
      <div className={componentStyles}>
        {this.renderLabelAndTicker()}
        {detail && <div className={styles.detail}>{detail}</div>}
      </div>
    );
  }
}

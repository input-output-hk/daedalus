// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownOption.scss';
import StakePool from '../../../domains/StakePool';
import LoadingSpinner from '../LoadingSpinner';

export type WalletOption = {
  delegatedStakePool?: ?StakePool,
  detail?: string,
  label: string,
  numberOfStakePools?: number,
  selected?: boolean,
  isSyncing?: boolean,
  syncingLabel?: string,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  renderLabel = (label: string) => {
    return (
      <div className={styles.label}>{label}</div>
    );
  };

  renderLabelSyncing = (label: string, syncingLabel: string) => {
    return (
      <div className={styles.label}>
        {label}
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
    } = this.props;
    if (!delegatedStakePool || !numberOfStakePools) {
      return (
        <div className={styles.topRow}>
          <div className={styles.topRowTicker}>
            {isSyncing
              ? this.renderLabelSyncing(label, syncingLabel)
              : this.renderLabel(label)
            }
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
          <div className={styles.label}>{label}</div>
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

// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownOption.scss';
import StakePool from '../../../domains/StakePool';

export type WalletOption = {
  delegatedStakePool?: ?StakePool,
  detail: string,
  label: string,
  numberOfStakePools?: number,
  selected?: boolean,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  renderLabelAndTicker = () => {
    const { delegatedStakePool, label, numberOfStakePools } = this.props;
    if (!delegatedStakePool || !numberOfStakePools) {
      return <div className={styles.label}>{label}</div>;
    }

    const { ranking, ticker } = delegatedStakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div className={styles.topRow}>
        <div style={{ color }} className={styles.ticker}>
          [{ticker}]
        </div>
        <div className={styles.label}>{label}</div>
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
        <div className={styles.detail}>{detail}</div>
      </div>
    );
  }
}

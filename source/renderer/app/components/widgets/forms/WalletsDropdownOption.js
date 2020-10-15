// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import SVGInline from 'react-svg-inline';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownOption.scss';
import StakePool from '../../../domains/StakePool';
import hardwareWalletsIcon from '../../../assets/images/hardware-wallet/connect-ic.inline.svg';

export type WalletOption = {
  delegatedStakePool?: ?StakePool,
  detail: string,
  label: string,
  numberOfStakePools?: number,
  selected?: boolean,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  renderLabelAndTicker = () => {
    const { delegatedStakePool, label, numberOfStakePools, isHardwareWallet } = this.props;
    if (!delegatedStakePool || !numberOfStakePools) {
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
    }

    const { ranking, ticker } = delegatedStakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div className={styles.topRow}>
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

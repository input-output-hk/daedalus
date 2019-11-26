// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import { getColorFromRange } from '../../../utils/colors';
import styles from './WalletsDropdownOption.scss';
import type { StakePool } from '../../../api/staking/types';

export type WalletOption = {
  activeDelegation?: StakePool,
  detail: string,
  label: string,
  numberOfStakePools?: number,
  selected?: boolean,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  renderActiveDelegationSlug = () => {
    const { activeDelegation, numberOfStakePools } = this.props;
    if (!activeDelegation || !numberOfStakePools) return null;

    const { ranking, slug } = activeDelegation;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div style={{ color }} className={styles.slug}>
        [{slug}]
      </div>
    );
  };

  render() {
    const { label, detail, selected } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
    });
    return (
      <div className={componentStyles}>
        {this.renderActiveDelegationSlug()}
        <div className={styles.label}>{label}</div>
        <div className={styles.detail}>{detail}</div>
      </div>
    );
  }
}

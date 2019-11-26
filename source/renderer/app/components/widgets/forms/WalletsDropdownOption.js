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
  renderLabelAndSlug = () => {
    const { activeDelegation, label, numberOfStakePools } = this.props;
    if (!activeDelegation || !numberOfStakePools) {
      return <div className={styles.label}>{label}</div>;
    }

    const { ranking, slug } = activeDelegation;
    const color = getColorFromRange(ranking, numberOfStakePools);
    return (
      <div className={styles.topRow}>
        <div style={{ color }} className={styles.slug}>
          [{slug}]
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
        {this.renderLabelAndSlug()}
        <div className={styles.detail}>{detail}</div>
      </div>
    );
  }
}

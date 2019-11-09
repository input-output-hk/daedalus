// @flow
import React, { Component } from 'react';
import classnames from 'classnames';
import styles from './WalletsDropdownOption.scss';

export type WalletOption = {
  label: string,
  detail: string,
  selected?: boolean,
};

export default class WalletsDropdownOption extends Component<WalletOption> {
  render() {
    const { label, detail, selected } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.label}>{label}</div>
        <div className={styles.detail}>{detail}</div>
      </div>
    );
  }
}

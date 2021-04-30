// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classnames from 'classnames';
import styles from './ItemDropdownOption.scss';

export type ItemDropdown = {
  label: string | Node,
  detail?: string | Node,
  selected?: boolean,
};

export default class ItemDropdownOption extends Component<ItemDropdown> {
  render() {
    const { label, detail, selected } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.label}>{label}</div>
        {detail && <div className={styles.detail}>{detail}</div>}
      </div>
    );
  }
}

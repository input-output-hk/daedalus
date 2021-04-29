// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import classnames from 'classnames';
import styles from './ItemDropdownOption.scss';

export type ItemDropdown = {
  topLabel: string | Node,
  bottomLabel?: string | Node,
  selected?: boolean,
  // isSyncing?: boolean,
};

export default class ItemDropdownOption extends Component<ItemDropdown> {
  render() {
    const { topLabel, bottomLabel, selected } = this.props;
    const componentStyles = classnames(styles.component, {
      [styles.selected]: selected,
    });
    return (
      <div className={componentStyles}>
        <div className={styles.topLabel}>{topLabel}</div>
        {bottomLabel && <div className={styles.bottomLabel}>{bottomLabel}</div>}
      </div>
    );
  }
}

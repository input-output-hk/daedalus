import React, { Component } from 'react';
import type { Element } from 'react';
import { Checkbox } from 'react-toolbox';
import styles from './CheckboxWithLongLabel.scss';

export default class CheckboxWithLongLabel extends Component {

  props: {
    label: string | Element<any>,
    checked: boolean,
    onChange: Function
  };

  render() {
    const { label, checked, onChange } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.checkbox}>
          <Checkbox
            checked={checked}
            onChange={onChange}
          />
        </div>
        <div className={styles.checkboxLabel}>{label}</div>
      </div>
    );
  }
}

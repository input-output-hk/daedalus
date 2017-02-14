import React, { Component, PropTypes } from 'react';
import { Checkbox } from 'react-toolbox';
import styles from './CheckboxWithLongLabel.scss';

export default class CheckboxWithLongLabel extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired,
    checked: PropTypes.bool.isRequired,
    onChange: PropTypes.func.isRequired
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

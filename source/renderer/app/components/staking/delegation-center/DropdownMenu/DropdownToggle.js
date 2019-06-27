// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './DropdownToggle.scss';

type Props = {
  label: any,
  onClick: Function,
};

@observer
export default class DropdownToggle extends Component<Props> {
  render() {
    const { label, onClick } = this.props;
    return (
      <button className={styles.component} onClick={onClick}>
        <div className={styles.label}>{label}</div>
      </button>
    );
  }
}

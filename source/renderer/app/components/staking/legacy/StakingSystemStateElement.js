// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingSystemStateElement.scss';

type Props = {
  value: string,
  label: string,
};

@observer
export default class StakingSystemState extends Component<Props> {
  render() {
    const { value, label } = this.props;
    return (
      <div className={styles.component}>
        <span className={styles.value}>{value}</span> {label}
      </div>
    );
  }
}

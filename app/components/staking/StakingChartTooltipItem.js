// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingChartTooltipItem.scss';

@observer
export default class StakingChartTooltipItem extends Component {

  props: {
    value: string,
    label: string
  };

  render() {
    const { value, label } = this.props;
    return (
      <div className={styles.component}>
        <span className={styles.value}>{value}</span> {label}
      </div>
    );
  }

}

import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingChartTooltipItem.scss';

type Props = {
  value: string;
  label: string;
};

class StakingChartTooltipItem extends Component<Props> {
  render() {
    const { value, label } = this.props;
    return (
      <div className={styles.component}>
        <span className={styles.value}>{value}</span> {label}
      </div>
    );
  }
}

export default observer(StakingChartTooltipItem);

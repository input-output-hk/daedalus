// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingChartTooltipItem.scss';

@observer
export default class StakingChartTooltipItem extends Component {

  static propTypes = {
    value: PropTypes.string.isRequired,
    label: PropTypes.string.isRequired
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

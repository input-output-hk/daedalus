// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingRewards.scss';

@observer
export default class StakingRewards extends Component<Props> {
  render() {
    return <div className={styles.component}>StakingRewards</div>;
  }
}

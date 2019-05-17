// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingStakePools.scss';

@observer
export default class StakingStakePools extends Component<Props> {
  render() {
    return <div className={styles.component}>StakingStakePools</div>;
  }
}

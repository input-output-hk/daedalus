// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingInfo.scss';

@observer
export default class StakingInfo extends Component<Props> {
  render() {
    return <div className={styles.component}>StakingInfo</div>;
  }
}

// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './StakingEpochs.scss';

@observer
export default class StakingEpochs extends Component<Props> {
  render() {
    return <div className={styles.component}>StakingEpochs</div>;
  }
}

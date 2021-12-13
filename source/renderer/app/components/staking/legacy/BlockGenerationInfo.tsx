// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './BlockGenerationInfo.scss';

@observer
export default class BlockGenerationInfo extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <div className={styles.heading}>
          You will be generating next block in
        </div>
        <div className={styles.timeLeft}>3 hours 10 minutes</div>
        <div className={styles.info}>
          be online and you will be rewarded or you can{' '}
          <span className={styles.link}>delegate</span> this process to the pool
        </div>
      </div>
    );
  }
}

import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './BlockGenerationInfo.scss' or... Remove this comment to see the full error message
import styles from './BlockGenerationInfo.scss';

@observer
class BlockGenerationInfo extends Component<any> {
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

export default BlockGenerationInfo;

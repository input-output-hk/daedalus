import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BlockGenerationInfo from './BlockGenerationInfo';
import StakingSwitch from './StakingSwitch';
import StakingSystemState from './StakingSystemState';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './Staking.scss' or its corresp... Remove this comment to see the full error message
import styles from './Staking.scss';

@observer
class Settings extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <div className={styles.leftSide}>
          <div className={styles.mainContent}>
            <BlockGenerationInfo />
          </div>
        </div>
        <div className={styles.rightSide}>
          <StakingSwitch active />
          <StakingSystemState />
        </div>
      </div>
    );
  }
}

export default Settings;

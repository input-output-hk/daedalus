// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BlockGenerationInfo from './BlockGenerationInfo';
import StakingSwitch from './StakingSwitch';
import StakingSystemState from './StakingSystemState';
import styles from './Staking.scss';

@observer
export default class Settings extends Component<any> {
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

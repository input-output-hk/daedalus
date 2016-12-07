// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import BlockGenerationInfo from './BlockGenerationInfo';
import StakingSwitch from './StakingSwitch';
import StakingSystemState from './StakingSystemState';
import styles from './Staking.scss';

@observer
export default class Settings extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired,
  };

  render() {
    const { children } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.leftSide}>
          <div className={styles.mainContent}>
            <BlockGenerationInfo />
          </div>
          <div className={styles.mainContent}>
            <div style={{ padding: '20px 0' }}>
              <div className={styles.slots}>Slots</div>
              {children}
            </div>
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

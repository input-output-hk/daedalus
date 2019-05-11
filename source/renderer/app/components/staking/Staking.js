// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BlockGenerationInfo from './BlockGenerationInfo';
import styles from './Staking.scss';

@observer
export default class Staking extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <BlockGenerationInfo />
        </div>
      </div>
    );
  }
}

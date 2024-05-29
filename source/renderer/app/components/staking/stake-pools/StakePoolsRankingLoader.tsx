import React, { Component } from 'react';
import { observer } from 'mobx-react';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './StakePoolsRankingLoader.scss';

class StakePoolsRankingLoader extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <LoadingSpinner big />
      </div>
    );
  }
}

export default observer(StakePoolsRankingLoader);

// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import styles from './StakePoolsRankingLoader.scss';

@observer
export default class StakePoolsRankingLoader extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <LoadingSpinner big />
      </div>
    );
  }
}

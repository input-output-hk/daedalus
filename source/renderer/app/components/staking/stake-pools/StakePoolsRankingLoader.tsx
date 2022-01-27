import React, { Component } from 'react';
import { observer } from 'mobx-react';
import LoadingSpinner from '../../widgets/LoadingSpinner';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './StakePoolsRankingLoader.scss... Remove this comment to see the full error message
import styles from './StakePoolsRankingLoader.scss';

@observer
class StakePoolsRankingLoader extends Component<any> {
  render() {
    return (
      <div className={styles.component}>
        <LoadingSpinner big />
      </div>
    );
  }
}

export default StakePoolsRankingLoader;

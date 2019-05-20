// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationProgressInfo from './DelegationProgressInfo';
import styles from './DelegationProgress.scss';

type Props = { percentage: number };

@observer
export default class DelegationProgress extends Component<Props> {
  render() {
    const { percentage } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <DelegationProgressInfo percentage={percentage} />
        </div>
      </div>
    );
  }
}

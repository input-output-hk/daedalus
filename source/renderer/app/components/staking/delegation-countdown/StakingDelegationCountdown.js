// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationInfo from './DelegationInfo';
import styles from './Delegation.scss';

type Props = { currentLocale: string, startDateTime: string };

@observer
export default class StakingDelegationCountdown extends Component<Props> {
  render() {
    const { currentLocale, startDateTime } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <DelegationInfo
            currentLocale={currentLocale}
            startDateTime={startDateTime}
          />
        </div>
      </div>
    );
  }
}

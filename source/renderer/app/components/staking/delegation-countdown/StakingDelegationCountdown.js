// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationCountdownInfo from './DelegationCountdownInfo';
import styles from './StakingDelegationCountdown.scss';

type Props = {
  redirectToStakingInfo?: Function,
  currentLocale: string,
  startDateTime: string,
};

@observer
export default class StakingDelegationCountdown extends Component<Props> {
  render() {
    const { redirectToStakingInfo, currentLocale, startDateTime } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <DelegationCountdownInfo
            redirectToStakingInfo={redirectToStakingInfo}
            currentLocale={currentLocale}
            startDateTime={startDateTime}
          />
        </div>
      </div>
    );
  }
}

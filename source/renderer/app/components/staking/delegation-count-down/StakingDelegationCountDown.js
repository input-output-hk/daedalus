// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationCountDownInfo from './DelegationCountDownInfo';
import styles from './StakingDelegationCountDown.scss';

type Props = { actions: any, currentLocale: string, startDateTime: string };

@observer
export default class StakingDelegationCountDown extends Component<Props> {
  render() {
    const { actions, currentLocale, startDateTime } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <DelegationCountDownInfo
            actions={actions}
            currentLocale={currentLocale}
            startDateTime={startDateTime}
          />
        </div>
      </div>
    );
  }
}

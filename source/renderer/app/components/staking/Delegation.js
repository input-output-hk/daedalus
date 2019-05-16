// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationInfo from './DelegationInfo';
import styles from './Delegation.scss';

type Props = { currentLocale: string, timeLeft?: number };

@observer
export default class Delegation extends Component<Props> {
  render() {
    const { currentLocale, timeLeft } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.mainContent}>
          <DelegationInfo currentLocale={currentLocale} timeLeft={timeLeft} />
        </div>
      </div>
    );
  }
}

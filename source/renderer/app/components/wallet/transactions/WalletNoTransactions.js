// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletNoTransactions.scss';

type Props = { label: string };

@observer
export default class WalletNoTransactions extends Component<Props> {
  render() {
    const { label } = this.props;

    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
      </div>
    );
  }
}

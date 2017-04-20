// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletNoTransactions.scss';

@observer
export default class WalletNoTransactions extends Component {

  props: {
    label: string,
  };

  render() {
    return (
      <div className={styles.component}>
        <div className={styles.label}>{this.props.label}</div>
      </div>
    );
  }
}

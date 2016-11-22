// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletNoTransactions.scss';

@observer
export default class WalletNoTransactions extends Component {

  static propTypes = {
    label: PropTypes.string.isRequired
  };

  render() {
    return (
      <div className={styles.component}>
        <div className={styles.label}>{this.props.label}</div>
      </div>
    );
  }
}

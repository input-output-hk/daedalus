import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletNoTransactions.scss' o... Remove this comment to see the full error message
import styles from './WalletNoTransactions.scss';

type Props = {
  label: string;
};

@observer
class WalletNoTransactions extends Component<Props> {
  render() {
    const { label } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.label}>{label}</div>
      </div>
    );
  }
}

export default WalletNoTransactions;

import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletRecoveryPhraseMnemonic.scss';

type Props = {
  phrase: string;
};

class WalletRecoveryPhraseMnemonic extends Component<Props> {
  render() {
    const { phrase } = this.props;
    return <div className={styles.component}>{phrase}</div>;
  }
}

export default observer(WalletRecoveryPhraseMnemonic);

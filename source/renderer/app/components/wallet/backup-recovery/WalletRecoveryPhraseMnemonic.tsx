import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryPhraseMnemonic... Remove this comment to see the full error message
import styles from './WalletRecoveryPhraseMnemonic.scss';

type Props = {
  phrase: string;
};

@observer
class WalletRecoveryPhraseMnemonic extends Component<Props> {
  render() {
    const { phrase } = this.props;
    return <div className={styles.component}>{phrase}</div>;
  }
}

export default WalletRecoveryPhraseMnemonic;

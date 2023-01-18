import React, { Component } from 'react';
// @ts-ignore ts-migrate(2724) FIXME: '"react"' has no exported member named 'Element'. ... Remove this comment to see the full error message
import type { Element } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRecoveryInstructions.s... Remove this comment to see the full error message
import styles from './WalletRecoveryInstructions.scss';

type Props = {
  instructionsText: string | Element<any>;
};

@observer
class WalletRecoveryInstructions extends Component<Props> {
  render() {
    const { instructionsText } = this.props;
    return <div className={styles.component}>{instructionsText}</div>;
  }
}

export default WalletRecoveryInstructions;

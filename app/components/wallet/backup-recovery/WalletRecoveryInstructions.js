// @flow
import React, { Component } from 'react';
import type { Element } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletRecoveryInstructions.scss';

@observer
export default class WalletRecoveryInstructions extends Component {

  props: {
    instructionsText: string | Element<any>,
  };

  render() {
    const { instructionsText } = this.props;
    return (
      <div className={styles.component}>{instructionsText}</div>
    );
  }

}

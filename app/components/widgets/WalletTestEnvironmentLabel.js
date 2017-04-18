import React, { Component } from 'react';
import styles from './WalletTestEnvironmentLabel.scss';

export default class WalletTestEnvironmentLabel extends Component {

  props: {
    version: number,
  };

  render() {
    const { version } = this.props;
    return (
      <div className={styles.component}>
        Testnet v{version}
      </div>
    );
  }

}

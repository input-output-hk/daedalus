import React, { Component, PropTypes } from 'react';
import styles from './WalletTestEnvironmentLabel.scss';

export default class WalletTestEnvironmentLabel extends Component {

  static propTypes = {
    version: PropTypes.number.isRequired,
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

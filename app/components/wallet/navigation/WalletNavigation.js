// @flow
import React, { Component } from 'react';
import styles from './WalletNavigation.scss';
import WalletNavHomeButton from './WalletHomeButton';
import WalletNavButton from './WalletNavButton';

export default class WalletNavigation extends Component {
  render() {
    return (
      <div className={styles.component}>

        <WalletNavHomeButton
          className={styles.walletButton}
          walletName="Test Wallet"
          amount={99.90}
          currency="$"
          isActive={false}
        />

        <WalletNavButton
          label="Send"
          normalIcon="./assets/images/send-ic.svg"
          activeIcon="./assets/images/send-white-ic.svg"
          className={styles.sendButton}
          isActive
        />

        <WalletNavButton
          label="Receive"
          normalIcon="./assets/images/receive-ic.svg"
          activeIcon="./assets/images/receive-white-ic.svg"
          className={styles.receiveButton}
          isActive={false}
        />

      </div>
    );
  }
}

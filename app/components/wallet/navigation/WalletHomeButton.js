// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletHomeButton.scss';

@observer
export default class WalletHomeButton extends Component {

  static propTypes = {
    walletName: PropTypes.string.isRequired,
    amount: PropTypes.number.isRequired,
    currency: PropTypes.string.isRequired,
    isActive: PropTypes.bool,
    onClick: PropTypes.func
  };

  render() {
    const { isActive, onClick, walletName, amount, currency } = this.props;
    return (
      <button className={isActive ? styles.active : styles.normal} onClick={onClick} >
        <div className={styles.container}>
          <div className={styles.walletName}>
            {walletName}
          </div>
          <div className={styles.walletAmount}>
            {amount} {currency}
          </div>
        </div>
      </button>
    );
  }
}

// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

@observer
export default class WalletLayoutWithNavigation extends Component {

  static propTypes = {
    children: PropTypes.arrayOf(PropTypes.element).isRequired,
    wallet: React.PropTypes.shape({
      name: PropTypes.string.isRequired,
      amount: PropTypes.number.isRequired,
      currency: PropTypes.string.isRequired,
    }),
  };

  render() {
    const { wallet, children } = this.props;
    return (
      <div className={styles.component}>
        <WalletNavigation wallet={wallet} />
        {children}
      </div>
    );
  }
}

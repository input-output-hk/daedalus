// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

@observer
export default class WalletWithNavigation extends Component {

  static propTypes = {
    children: PropTypes.element.isRequired,
    isActiveScreen: PropTypes.func.isRequired,
    onWalletNavItemClick: PropTypes.func
  };

  render() {
    const { children, isActiveScreen, onWalletNavItemClick } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.navigation}>
          <WalletNavigation
            isActiveNavItem={isActiveScreen}
            onNavItemClick={onWalletNavItemClick}
          />
        </div>
        <div className={styles.page}>
          {children}
        </div>
      </div>
    );
  }
}

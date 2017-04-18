// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

@observer
export default class WalletWithNavigation extends Component {

  props: {
    children?: Children,
    isActiveScreen: Function,
    onWalletNavItemClick: Function,
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

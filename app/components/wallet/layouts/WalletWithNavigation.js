// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

type Props = {
  children?: Node,
  isActiveScreen: Function,
  onWalletNavItemClick: Function,
};

@observer
export default class WalletWithNavigation extends Component<Props> {

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

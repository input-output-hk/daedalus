// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';

type Props = {
  children?: Node,
  activeItem: string,
  isActiveScreen: Function,
  isLegacy: boolean,
  onWalletNavItemClick: Function,
  hasNotification?: boolean,
};

@observer
export default class WalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      isActiveScreen,
      isLegacy,
      onWalletNavItemClick,
      activeItem,
      hasNotification,
    } = this.props;
    return (
      <div className={styles.component}>
        <div className={styles.navigation}>
          <WalletNavigation
            isActiveNavItem={isActiveScreen}
            isLegacy={isLegacy}
            onNavItemClick={onWalletNavItemClick}
            activeItem={activeItem}
            hasNotification={hasNotification}
          />
        </div>
        <div className={styles.page}>{children}</div>
      </div>
    );
  }
}

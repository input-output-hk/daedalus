// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';
import NotResponding from '../not-responding/NotResponding';
import SetWalletPassword from '../settings/SetWalletPassword';

type Props = {
  children?: Node,
  activeItem: string,
  hasNotification?: boolean,
  hasPassword: boolean,
  isActiveScreen: Function,
  isLegacy: boolean,
  isNotResponding: boolean,
  isSetWalletPasswordDialogOpen: boolean,
  onOpenExternalLink: Function,
  onRestartNode: Function,
  onSetWalletPassword: Function,
  onWalletNavItemClick: Function,
};

@observer
export default class WalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      activeItem,
      hasNotification,
      hasPassword,
      isActiveScreen,
      isLegacy,
      isNotResponding,
      isSetWalletPasswordDialogOpen,
      onOpenExternalLink,
      onRestartNode,
      onSetWalletPassword,
      onWalletNavItemClick,
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

        {!hasPassword && (
          <SetWalletPassword
            isSetWalletPasswordDialogOpen={isSetWalletPasswordDialogOpen}
            onSetWalletPassword={onSetWalletPassword}
          />
        )}

        {isNotResponding && (
          <NotResponding
            walletName={activeItem}
            onRestartNode={onRestartNode}
            onOpenExternalLink={onOpenExternalLink}
          />
        )}
      </div>
    );
  }
}

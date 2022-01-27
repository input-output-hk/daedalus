import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import classnames from 'classnames';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletWithNavigation.scss' o... Remove this comment to see the full error message
import styles from './WalletWithNavigation.scss';
import NotResponding from '../not-responding/NotResponding';
import SetWalletPassword from '../settings/SetWalletPassword';

type Props = {
  children?: Node;
  activeItem: string;
  hasNotification?: boolean;
  hasPassword: boolean;
  isActiveScreen: (...args: Array<any>) => any;
  isLegacy: boolean;
  isNotResponding: boolean;
  isHardwareWallet: boolean;
  isSetWalletPasswordDialogOpen: boolean;
  onOpenExternalLink: (...args: Array<any>) => any;
  onRestartNode: (...args: Array<any>) => any;
  onSetWalletPassword: (...args: Array<any>) => any;
  onWalletNavItemClick: (...args: Array<any>) => any;
};

@observer
class WalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      activeItem,
      hasNotification,
      hasPassword,
      isActiveScreen,
      isLegacy,
      isNotResponding,
      isHardwareWallet,
      isSetWalletPasswordDialogOpen,
      onOpenExternalLink,
      onRestartNode,
      onSetWalletPassword,
      onWalletNavItemClick,
    } = this.props;
    const componentStyles = classnames([styles.component, styles[activeItem]]);
    return (
      <div className={componentStyles}>
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

        {!hasPassword && !isHardwareWallet && (
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

export default WalletWithNavigation;

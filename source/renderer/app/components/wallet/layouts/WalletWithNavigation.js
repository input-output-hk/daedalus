// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';
import NotResponding from '../not-responding/NotResponding';
import ChangeSpendingPasswordDialog from '../settings/ChangeSpendingPasswordDialog';
import ChangeSpendingPasswordDialogContainer
  from '../../../containers/wallet/dialogs/settings/ChangeSpendingPasswordDialogContainer';

type Props = {
  children?: Node,
  activeItem: string,
  isActiveScreen: Function,
  isLegacy: boolean,
  onWalletNavItemClick: Function,
  hasPassword: boolean,
  onRestartNode: Function,
  onOpenExternalLink: Function,
  hasNotification?: boolean,
  isNotResponding: boolean,
  isDialogOpen: boolean,
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
      isNotResponding,
      hasPassword,
      onRestartNode,
      onOpenExternalLink,
      isDialogOpen,
    } = this.props;

    return (
      <div className={styles.component}>
        {isNotResponding && (
          <NotResponding
            walletName={activeItem}
            onRestartNode={onRestartNode}
            onOpenExternalLink={onOpenExternalLink}
          />
        )}
        {!hasPassword && (
          <SetWalletPassword
            walletName={activeItem}
            onRestartNode={onRestartNode}
            onOpenExternalLink={onOpenExternalLink}
          />
        )}
        {isDialogOpen(ChangeSpendingPasswordDialog) ? (
          <ChangeSpendingPasswordDialogContainer forceSetPassword />
        ) : (
          false
        )}
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

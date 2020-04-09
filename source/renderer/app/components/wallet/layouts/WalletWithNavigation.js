// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import WalletNavigation from '../navigation/WalletNavigation';
import styles from './WalletWithNavigation.scss';
import NotResponding from '../not-responding/NotResponding';
import ChangeSpendingPasswordDialog from '../settings/ChangeSpendingPasswordDialog';
import Wallet from '../../../domains/Wallet';
import SetWalletPassword from '../settings/SetWalletPassword';
import DialogsActions from '../../../actions/dialogs-actions';

type Props = {
  children?: Node,
  activeItem: string,
  isActiveScreen: Function,
  onWalletNavItemClick: Function,
  onRestartNode: Function,
  onOpenExternalLink: Function,
  hasNotification?: boolean,
  isDialogOpen: Function,
  activeWallet: Wallet,
  dialogs: DialogsActions,
};

@observer
export default class WalletWithNavigation extends Component<Props> {
  render() {
    const {
      children,
      isActiveScreen,
      onWalletNavItemClick,
      activeItem,
      hasNotification,
      onRestartNode,
      onOpenExternalLink,
      isDialogOpen,
      activeWallet,
      dialogs,
    } = this.props;

    return (
      <div className={styles.component}>
        {activeWallet.isNotResponding && (
          <NotResponding
            walletName={activeItem}
            onRestartNode={onRestartNode}
            onOpenExternalLink={onOpenExternalLink}
          />
        )}
        {!activeWallet.hasPassword && (
          <SetWalletPassword
            onConfirm={() => {
              dialogs.open.trigger({
                dialog: ChangeSpendingPasswordDialog,
              });
            }}
            isDialogOpen={isDialogOpen}
          />
        )}
        <div className={styles.navigation}>
          <WalletNavigation
            isActiveNavItem={isActiveScreen}
            isLegacy={activeWallet.isLegacy}
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

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
import Wallet from '../../../domains/Wallet';
import SetWalletPassword from '../settings/SetWalletPassword';
import DialogsActions from '../../../actions/dialogs-actions';

type Props = {
  children?: Node,
  activeItem: string,
  isActiveScreen: Function,
  onWalletNavItemClick: Function,
  hasPassword: boolean,
  onRestartNode: Function,
  onOpenExternalLink: Function,
  hasNotification?: boolean,
  isDialogOpen: boolean,
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
      hasPassword,
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
        {!hasPassword && (
          <SetWalletPassword
            onConfirm={() => {
              dialogs.closeActiveDialog.trigger();
              dialogs.open.trigger({
                dialog: ChangeSpendingPasswordDialog,
              });
            }}
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

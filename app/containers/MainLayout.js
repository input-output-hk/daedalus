// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../components/wallet/WalletRestoreDialog';
import WalletBackupPage from './wallet/WalletBackupPage';
import WalletAddPage from './wallet/WalletAddPage';
import Wallet from '../domain/Wallet';
import Request from '../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet),
        isAddWalletDialogOpen: PropTypes.bool.isRequired,
        isCreateWalletDialogOpen: PropTypes.bool.isRequired,
        isWalletRestoreDialogOpen: PropTypes.bool.isRequired,
        walletBackup: PropTypes.shape({
          inProgress: PropTypes.bool.isRequired
        }),
        restoreRequest: PropTypes.instanceOf(Request).isRequired,
        isValidMnemonic: PropTypes.func.isRequired
      }).isRequired,
      walletBackup: PropTypes.shape({
        inProgress: PropTypes.bool.isRequired,
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
      createPersonalWallet: PropTypes.func.isRequired,
      toggleAddWallet: PropTypes.func.isRequired,
      toggleWalletRestore: PropTypes.func.isRequired,
      restoreWallet: PropTypes.func.isRequired,
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit = (values: Object) => {
    this.props.actions.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  };

  handleRestoreWalletSubmit = (values: Object) => {
    this.props.actions.restoreWallet(values);
  };

  routeToWallet = (walletId: string) => {
    const { actions, stores } = this.props;
    actions.goToRoute({ route: stores.wallets.getWalletRoute(walletId) });
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, wallets } = stores;
    const { restoreRequest } = wallets;
    const { toggleAddWallet, toggleCreateWalletDialog, toggleWalletRestore } = actions;
    const activeWallet = stores.wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const isWalletBackupInProgress = this.props.stores.walletBackup.inProgress;

    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: toggleAddWallet,
          onWalletItemClick: this.routeToWallet,
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        isMaximized={sidebar.isMaximized}
        categories={sidebar.CATEGORIES}
        currentCategory={sidebar.currentCategory}
        onCategoryClicked={category => actions.sidebarCategorySelected({ category })}
        activeWalletId={activeWalletId}
      />
    );
    const appbar = <AppBar onToggleSidebar={actions.toggleSidebar} />;
    const addWalletRestoreDialog = wallets.isWalletRestoreDialogOpen ? (
      <WalletRestoreDialog
        onSubmit={this.handleRestoreWalletSubmit}
        onCancel={toggleWalletRestore}
        error={restoreRequest.error}
        mnemonicValidator={mnemonic => this.props.stores.wallets.isValidMnemonic(mnemonic)}
      />
    ) : null;
    const addWalletDialog = wallets.isAddWalletDialogOpen ? (<WalletAddPage />) : null;
    const createWalletDialog = wallets.isCreateWalletDialogOpen ? (
      <WalletCreateDialog
        onSubmit={this.handleAddWalletSubmit}
        onCancel={toggleCreateWalletDialog}
      />
    ) : null;
    const addWalletBackupDialog = isWalletBackupInProgress ? (<WalletBackupPage />) : null;
    return (
      <SidebarLayout sidebar={sidebarComponent} appbar={appbar}>
        {this.props.children}
        {addWalletDialog}
        {createWalletDialog}
        {addWalletRestoreDialog}
        {addWalletBackupDialog}
      </SidebarLayout>
    );
  }
}

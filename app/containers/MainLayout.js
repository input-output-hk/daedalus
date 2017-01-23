// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import WalletImportDialog from '../components/wallet/WalletImportDialog';
import WalletBackupPage from './wallet/WalletBackupPage';
import WalletAddPage from './wallet/WalletAddPage';
import Wallet from '../domain/Wallet';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet),
        isAddWalletDialogOpen: PropTypes.bool.isRequired,
        isCreateWalletDialogOpen: PropTypes.bool.isRequired,
        isWalletImportDialogOpen: PropTypes.bool.isRequired,
        walletBackup: PropTypes.shape({
          inProgress: PropTypes.bool.isRequired
        }),
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
      createPersonalWallet: PropTypes.func.isRequired,
      toggleAddWallet: PropTypes.func.isRequired,
      toggleWalletImport: PropTypes.func.isRequired,
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit = (values: Object) => {
    this.props.actions.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  };

  handleAddWalletImport = (values: Object) => {
    console.log(values);
  };

  routeToWallet = (walletId) => {
    const { actions, stores } = this.props;
    actions.goToRoute({ route: stores.wallets.getWalletRoute(walletId) });
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, wallets } = stores;
    const { toggleAddWallet, toggleCreateWalletDialog, toggleWalletImport } = actions;
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
        route={sidebar.route}
        menus={sidebarMenus}
        hidden={sidebar.hidden}
        isMaximized={sidebar.isMaximized}
        onCategoryClicked={route => actions.changeSidebarRoute({ route })}
        activeWalletId={activeWalletId}
      />
    );
    const appbar = <AppBar onToggleSidebar={actions.toggleSidebar} />;
    const addWalletImportDialog = wallets.isWalletImportDialogOpen ? (
      <WalletImportDialog
        onSubmit={this.handleAddWalletImport}
        onCancel={toggleWalletImport}
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
        {addWalletImportDialog}
        {addWalletBackupDialog}
      </SidebarLayout>
    );
  }
}

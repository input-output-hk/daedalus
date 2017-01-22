// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import WalletBackupPage from './wallet/WalletBackupPage';
import WalletAddPage from './wallet/WalletAddPage';
import WalletImportPage from './wallet/WalletImportPage';
import Wallet from '../domain/Wallet';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      sidebar: PropTypes.shape({
        isAddWalletDialogOpen: PropTypes.bool.isRequired,
        isCreateWalletDialogOpen: PropTypes.bool.isRequired,
        isWalletImportDialogOpen: PropTypes.bool.isRequired,
      }).isRequired,
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet),
        walletBackup: PropTypes.shape({
          inProgress: PropTypes.bool.isRequired
        })
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
      createPersonalWallet: PropTypes.func.isRequired,
      toggleAddWallet: PropTypes.func.isRequired,
      toggleCreateWalletDialog: PropTypes.func.isRequired
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit = (values: Object) => {
    this.props.actions.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  };

  routeToWallet = (walletId) => {
    const { actions, stores } = this.props;
    actions.goToRoute({ route: stores.wallets.getWalletRoute(walletId) });
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar } = stores;
    const { toggleAddWallet, toggleCreateWalletDialog } = actions;
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
    const addWalletImportDialog = sidebar.isWalletImportDialogOpen ? (<WalletImportPage />) : null;
    const addWalletDialog = sidebar.isAddWalletDialogOpen ? (<WalletAddPage />) : null;
    const createWalletDialog = sidebar.isCreateWalletDialogOpen ? (
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

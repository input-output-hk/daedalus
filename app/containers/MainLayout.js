// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import AppBar from '../components/layout/AppBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import SidebarLayout from '../components/layout/SidebarLayout';
import { oneOrManyChildElements } from '../propTypes';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../components/wallet/WalletRestoreDialog';
import WalletBackupPage from './wallet/WalletBackupPage';
import WalletAddPage from './wallet/WalletAddPage';
import WalletKeyImportPage from './wallet/WalletKeyImportPage';
import NodeUpdatePage from './notifications/NodeUpdatePage';
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
        isWalletKeyImportDialogOpen: PropTypes.bool.isRequired,
        walletBackup: PropTypes.shape({
          inProgress: PropTypes.bool.isRequired
        }),
        restoreRequest: PropTypes.instanceOf(Request).isRequired,
        isValidMnemonic: PropTypes.func.isRequired
      }).isRequired,
      walletBackup: PropTypes.shape({
        inProgress: PropTypes.bool.isRequired,
      }).isRequired,
      networkStatus: PropTypes.shape({
        isSynced: PropTypes.bool.isRequired,
        isSyncing: PropTypes.bool.isRequired,
        syncPercentage: PropTypes.number.isRequired,
      }).isRequired,
      nodeUpdate: PropTypes.shape({
        isUpdateAvailable: PropTypes.bool.isRequired,
        isUpdatePostponed: PropTypes.bool.isRequired,
      }).isRequired,
      sidebar: PropTypes.shape({
        isHidden: PropTypes.bool.isRequired,
        isShowingSubMenus: PropTypes.bool.isRequired,
        CATEGORIES: PropTypes.shape({
          WALLETS: PropTypes.string.isRequired,
          ADA_REDEMPTION: PropTypes.string.isRequired,
        }).isRequired,
        currentCategory: PropTypes.string,
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      router: PropTypes.shape({
        goToRoute: PropTypes.func.isRequired,
      }),
      wallets: PropTypes.shape({
        create: PropTypes.func.isRequired,
        toggleAddWallet: PropTypes.func.isRequired,
        toggleWalletRestore: PropTypes.func.isRequired,
        restoreWallet: PropTypes.func.isRequired,
      }),
    }).isRequired,
    children: oneOrManyChildElements
  };

  handleAddWalletSubmit = (values: Object) => {
    this.props.actions.wallets.create({
      name: values.walletName,
      currency: values.currency,
    });
  };

  handleRestoreWalletSubmit = (values: Object) => {
    this.props.actions.wallets.restoreWallet(values);
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, wallets, networkStatus } = stores;
    const { restoreRequest, isWalletKeyImportDialogOpen } = wallets;
    const {
      toggleAddWallet, toggleCreateWalletDialog, toggleWalletRestore
    } = actions.wallets;
    const { isSynced, syncPercentage } = networkStatus;
    const activeWallet = stores.wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const isWalletBackupInProgress = this.props.stores.walletBackup.inProgress;
    const isNodeUpdateAvailable = this.props.stores.nodeUpdate.isUpdateAvailable;
    const isUpdatePostponed = this.props.stores.nodeUpdate.isUpdatePostponed;

    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        actions: {
          onAddWallet: toggleAddWallet,
          onWalletItemClick: (walletId: string) => {
            actions.sidebar.walletSelected({ walletId });
          }
        }
      }
    };
    const sidebarComponent = (
      <Sidebar
        menus={sidebarMenus}
        isHidden={sidebar.isHidden}
        isShowingSubMenus={sidebar.isShowingSubMenus}
        categories={sidebar.CATEGORIES}
        currentCategory={sidebar.currentCategory}
        onCategoryClicked={category => actions.sidebar.sidebarCategorySelected({ category })}
        activeWalletId={activeWalletId}
        isSynced={isSynced}
      />
    );
    const appbar = (
      <AppBar onToggleSidebar={actions.sidebar.toggleSidebar}>
        <NodeSyncStatusIcon isSynced={isSynced} syncPercentage={syncPercentage} />
      </AppBar>
    );
    const addWalletRestoreDialog = wallets.isWalletRestoreDialogOpen ? (
      <WalletRestoreDialog
        onSubmit={this.handleRestoreWalletSubmit}
        onCancel={toggleWalletRestore}
        error={restoreRequest.error}
        mnemonicValidator={mnemonic => this.props.stores.wallets.isValidMnemonic(mnemonic)}
      />
    ) : null;
    const addWalletDialog = (
      wallets.isAddWalletDialogOpen && !isWalletBackupInProgress ? <WalletAddPage /> : null
    );
    const createWalletDialog = wallets.isCreateWalletDialogOpen ? (
      <WalletCreateDialog
        onSubmit={this.handleAddWalletSubmit}
        onCancel={toggleCreateWalletDialog}
      />
    ) : null;
    const addWalletBackupDialog = (
      isWalletBackupInProgress ? <WalletBackupPage /> : null
    );
    const addNodeUpdateNotification = (
      isNodeUpdateAvailable && !isUpdatePostponed ? <NodeUpdatePage /> : null
    );
    const addWalletKeyImportDialog = (
      isWalletKeyImportDialogOpen ? <WalletKeyImportPage /> : null
    );
    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        appbar={appbar}
        notification={addNodeUpdateNotification}
      >
        {this.props.children}
        {addWalletDialog}
        {createWalletDialog}
        {addWalletRestoreDialog}
        {addWalletBackupDialog}
        {addWalletKeyImportDialog}
      </SidebarLayout>
    );
  }
}

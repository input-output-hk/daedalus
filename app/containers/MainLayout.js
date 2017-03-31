// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
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
        isShowingSubMenus: PropTypes.bool.isRequired,
        CATEGORIES: PropTypes.shape({
          WALLETS: PropTypes.string.isRequired,
          ADA_REDEMPTION: PropTypes.string.isRequired,
        }).isRequired,
        activeSidebarCategory: PropTypes.string,
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
    let activeDialog = null;

    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        activeWalletId,
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
        isShowingSubMenus={sidebar.isShowingSubMenus}
        categories={sidebar.CATEGORIES}
        activeSidebarCategory={sidebar.activeSidebarCategory}
        onCategoryClicked={category => actions.sidebar.activateSidebarCategory({ category })}
        isSynced={isSynced}
      />
    );

    const isProduction = false; // TODO: replace with getEnv Api call
    const testnetVersion = 0.3;
    const testEnvironmentLabel = (
      !isProduction ? <WalletTestEnvironmentLabel version={testnetVersion} /> : null
    );

    const topbar = (
      <TopBar onToggleSidebar={actions.sidebar.toggleSubMenus} activeWallet={activeWallet}>
        {testEnvironmentLabel}
        <NodeSyncStatusIcon
          isSynced={isSynced}
          syncPercentage={syncPercentage}
          isProduction={isProduction}
        />
      </TopBar>
    );

    // TODO: Refactor dialogs logic away from the layout

    if (wallets.isWalletRestoreDialogOpen) {
      activeDialog = (
        <WalletRestoreDialog
          onSubmit={this.handleRestoreWalletSubmit}
          onCancel={toggleWalletRestore}
          error={restoreRequest.error}
          mnemonicValidator={mnemonic => this.props.stores.wallets.isValidMnemonic(mnemonic)}
        />
      );
    } else if (wallets.isAddWalletDialogOpen && !isWalletBackupInProgress) {
      activeDialog = <WalletAddPage />;
    } else if (wallets.isCreateWalletDialogOpen) {
      activeDialog = (
        <WalletCreateDialog
          onSubmit={this.handleAddWalletSubmit}
          onCancel={toggleCreateWalletDialog}
        />
      );
    } else if (isWalletBackupInProgress) {
      activeDialog = <WalletBackupPage />;
    } else if (isWalletKeyImportDialogOpen) {
      activeDialog = <WalletKeyImportPage />;
    }

    const addNodeUpdateNotification = (
      isNodeUpdateAvailable && !isUpdatePostponed ? <NodeUpdatePage /> : null
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={topbar}
        notification={addNodeUpdateNotification}
        contentDialog={activeDialog}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }
}

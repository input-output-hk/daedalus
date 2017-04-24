// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import SidebarLayout from '../components/layout/SidebarLayout';
import WalletCreateDialog from '../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../components/wallet/WalletRestoreDialog';
import WalletBackupPage from './wallet/WalletBackupPage';
import WalletAddPage from './wallet/WalletAddPage';
import WalletKeyImportPage from './wallet/WalletKeyImportPage';
import NodeUpdatePage from './notifications/NodeUpdatePage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static defaultProps = { actions: null, stores: null, children: null };
  props: InjectedContainerProps;

  handleAddWalletSubmit = (values: Object) => {
    const walletData = {
      name: values.walletName,
      currency: values.currency,
      password: values.walletPassword,
    };
    this.props.actions.wallets.createWallet.trigger(walletData);
  };

  handleRestoreWalletSubmit = (values: Object) => {
    this.props.actions.wallets.restoreWallet.trigger(values);
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, wallets, networkStatus, app } = stores;
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
          onAddWallet: toggleAddWallet.trigger,
          onWalletItemClick: (walletId: string) => {
            actions.sidebar.walletSelected.trigger({ walletId });
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
        onCategoryClicked={category => {
          actions.sidebar.activateSidebarCategory.trigger({ category });
        }}
        isSynced={isSynced}
      />
    );

    const isProduction = false; // TODO: replace with getEnv Api call
    const testnetVersion = 0.3;
    const testEnvironmentLabel = (
      !isProduction ? <WalletTestEnvironmentLabel version={testnetVersion} /> : null
    );

    const topbar = (
      <TopBar
        onToggleSidebar={actions.sidebar.toggleSubMenus.trigger}
        activeWallet={activeWallet}
        currentRoute={app.currentRoute}
      >
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
          onCancel={toggleWalletRestore.trigger}
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
          onCancel={toggleCreateWalletDialog.trigger}
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

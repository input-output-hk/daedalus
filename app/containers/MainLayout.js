// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBar from '../components/layout/TopBar';
import NodeSyncStatusIcon from '../components/widgets/NodeSyncStatusIcon';
import WalletTestEnvironmentLabel from '../components/widgets/WalletTestEnvironmentLabel';
import SidebarLayout from '../components/layout/SidebarLayout';
import NodeUpdatePage from './notifications/NodeUpdatePage';
import WalletAddPage from './wallet/WalletAddPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static defaultProps = { actions: null, stores: null, children: null };
  props: InjectedContainerProps;

  render() {
    const { actions, stores } = this.props;
    const { sidebar, networkStatus, app } = stores;
    const { isSynced, syncPercentage } = networkStatus;
    const activeWallet = stores.wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const isNodeUpdateAvailable = this.props.stores.nodeUpdate.isUpdateAvailable;
    const isUpdatePostponed = this.props.stores.nodeUpdate.isUpdatePostponed;

    const sidebarMenus = {
      wallets: {
        items: sidebar.wallets,
        activeWalletId,
        actions: {
          onWalletItemClick: (walletId: string) => {
            actions.sidebar.walletSelected.trigger({ walletId });
          },
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
        openDialogAction={actions.dialogs.open.trigger}
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

    const addNodeUpdateNotification = (
      isNodeUpdateAvailable && !isUpdatePostponed ? <NodeUpdatePage /> : null
    );

    const addWalletDialog = (
      <WalletAddPage />
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={topbar}
        notification={addNodeUpdateNotification}
        contentDialog={addWalletDialog}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

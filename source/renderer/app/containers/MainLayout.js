// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
import StatusMessagesNotification from '../components/notifications/StatusMessagesNotification';
import NodeUpdatePage from './notifications/NodeUpdatePage';
import WalletAddPage from './wallet/WalletAddPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';

type Props = InjectedContainerProps;

@inject('stores', 'actions') @observer
export default class MainLayout extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  render() {
    const { actions, stores } = this.props;
    const { sidebar } = stores;
    const wallets = stores.ada.wallets;
    const activeWallet = wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const isNodeUpdateAvailable = this.props.stores.ada.nodeUpdate.isUpdateAvailable;
    const isUpdatePostponed = this.props.stores.ada.nodeUpdate.isUpdatePostponed;
    const { isImportActive, isRestoreActive } = wallets;

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
        isSynced
        openDialogAction={actions.dialogs.open.trigger}
      />
    );

    const addNodeUpdateNotification = (
      isNodeUpdateAvailable && !isUpdatePostponed ? <NodeUpdatePage /> : null
    );

    const addStatusMessagesNotification = (
      isImportActive || isRestoreActive ? (
        <StatusMessagesNotification
          isImportActive={isImportActive}
          isRestoreActive={isRestoreActive}
        />
      ) : null
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={<TopBarContainer />}
        notification={addStatusMessagesNotification || addNodeUpdateNotification}
        contentDialog={<WalletAddPage />}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

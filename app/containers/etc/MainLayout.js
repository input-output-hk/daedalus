// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../../components/sidebar/Sidebar';
import TopBarContainer from '../TopBarContainer';
import SidebarLayout from '../../components/layout/SidebarLayout';
import WalletAddPage from '../wallet/WalletAddPage';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import StatusMessagesNotification from '../../components/notifications/StatusMessagesNotification';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component<InjectedContainerProps> {

  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {}
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar } = stores;
    const wallets = stores.etc.wallets;
    const activeWallet = wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
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
        contentDialog={<WalletAddPage />}
        notification={addStatusMessagesNotification}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

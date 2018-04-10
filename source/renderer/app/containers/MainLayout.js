// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
import StatusMessagesNotification from '../components/notifications/StatusMessagesNotification';
import NodeUpdatePage from './notifications/NodeUpdatePage';
import WalletSupportRequestPage from './wallet/WalletSupportRequestPage';
import PaperWalletCreateCertificatePage from './wallet/PaperWalletCreateCertificatePage';
import type { InjectedContainerProps } from '../types/injectedPropsType';
import { ROUTES } from '../routes-config';

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
    const wallets = stores.ada.wallets;
    const activeWallet = wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const isNodeUpdateAvailable = this.props.stores.ada.nodeUpdate.isUpdateAvailable;
    const isUpdatePostponed = this.props.stores.ada.nodeUpdate.isUpdatePostponed;
    const { isImportActive, isRestoreActive } = wallets;

    const sidebarMenus = sidebar.wallets.length > 0 ? {
      wallets: {
        items: sidebar.wallets,
        activeWalletId,
        actions: {
          onWalletItemClick: (walletId: string) => {
            actions.sidebar.walletSelected.trigger({ walletId });
          },
        }
      }
    } : null;
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
        onAddWallet={() => actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD })}
        isDialogOpen={stores.uiDialogs.isOpen}
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
        contentDialogs={[
          <WalletSupportRequestPage key="WalletSupportRequestPage" />,
          <PaperWalletCreateCertificatePage
            key="PaperWalletCreateCertificatePage"
            certificateStep={this.props.stores.ada.wallets.certificateStep}
          />,
        ]}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

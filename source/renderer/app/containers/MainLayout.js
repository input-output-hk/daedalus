// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
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
        onSubmitSupportRequest={
          () => actions.router.goToRoute.trigger({ route: ROUTES.SETTINGS.SUPPORT })
        }
        pathname={this.props.stores.router.location.pathname}
      />
    );

    const addNodeUpdateNotification = (
      isNodeUpdateAvailable && !isUpdatePostponed ? <NodeUpdatePage /> : null
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={<TopBarContainer />}
        notification={addNodeUpdateNotification}
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

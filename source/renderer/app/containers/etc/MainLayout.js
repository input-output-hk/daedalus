// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../../components/sidebar/Sidebar';
import TopBarContainer from '../TopBarContainer';
import SidebarLayout from '../../components/layout/SidebarLayout';
import WalletSupportRequestPage from '../wallet/WalletSupportRequestPage';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import { ROUTES } from '../../routes-config';

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
      />
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={<TopBarContainer />}
        contentDialogs={[
          <WalletSupportRequestPage key="WalletSupportRequestPage" />,
        ]}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

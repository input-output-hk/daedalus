// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../../components/sidebar/Sidebar';
import TopBarContainer from '../TopBarContainer';
import SidebarLayout from '../../components/layout/SidebarLayout';
import WalletAddPage from '../wallet/WalletAddPage';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class MainLayout extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };
  props: InjectedContainerProps;

  render() {
    const { actions, stores } = this.props;
    const { sidebar } = stores;
    const activeWallet = stores.etc.wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;

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

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={<TopBarContainer />}
        contentDialog={<WalletAddPage />}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }

}

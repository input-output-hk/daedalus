// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
import PaperWalletCreateCertificatePage from './wallet/PaperWalletCreateCertificatePage';
import TransferFundsPage from './wallet/TransferFundsPage';
import type { InjectedContainerProps } from '../types/injectedPropsType';
import { ROUTES } from '../routes-config';

@inject('stores', 'actions')
@observer
export default class MainLayout extends Component<InjectedContainerProps> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, wallets, profile, app } = stores;
    const activeWallet = wallets.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const { currentTheme } = profile;
    const {
      environment: { network },
    } = app;

    const sidebarMenus =
      sidebar.wallets.length > 0
        ? {
            wallets: {
              items: sidebar.wallets,
              activeWalletId,
              actions: {
                onWalletItemClick: (walletId: string) => {
                  actions.sidebar.walletSelected.trigger({ walletId });
                },
              },
            },
          }
        : null;

    const sidebarComponent = (
      <Sidebar
        menus={sidebarMenus}
        isShowingSubMenus={sidebar.isShowingSubMenus}
        isIncentivizedTestnet={global.isIncentivizedTestnet}
        categories={sidebar.CATEGORIES}
        activeSidebarCategory={sidebar.activeSidebarCategory}
        onActivateCategory={category => {
          actions.sidebar.activateSidebarCategory.trigger({ category });
        }}
        onOpenDialog={dialog => actions.dialogs.open.trigger({ dialog })}
        onAddWallet={() =>
          actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD })
        }
        onSubmitSupportRequest={() =>
          actions.router.goToRoute.trigger({ route: ROUTES.SETTINGS.SUPPORT })
        }
        onOpenSplashNetwork={() => actions.networkStatus.toggleSplash.trigger()}
        pathname={this.props.stores.router.location.pathname}
        currentTheme={currentTheme}
        network={network}
      />
    );

    return (
      <SidebarLayout
        sidebar={sidebarComponent}
        topbar={<TopBarContainer />}
        contentDialogs={[
          <PaperWalletCreateCertificatePage
            key="PaperWalletCreateCertificatePage"
            certificateStep={this.props.stores.wallets.certificateStep}
          />,
          <TransferFundsPage key="TransferFundsPage" />,
        ]}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }
}

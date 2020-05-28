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
    const { sidebar, profile, app, wallets: walletsStore } = stores;
    const { isHardwareWalletRoute } = walletsStore;
    const activeWallet = walletsStore.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const activeHardwareWallet = walletsStore.activeHardwareWallet;
    const activeHardwareWalletId = activeHardwareWallet ? activeHardwareWallet.id : null;

    console.debug('>>> activeHardwareWallet: ', activeHardwareWallet);

    const { currentTheme } = profile;
    const {
      environment: { network, isDev },
    } = app;

    const appWallets =
      sidebar.wallets.length > 0
        ? {
            items: sidebar.wallets,
            activeWalletId,
            actions: {
              onWalletItemClick: (walletId: string) => {
                actions.sidebar.walletSelected.trigger({ walletId });
              },
            },
          }
        : null;

    const hardwareWallets =
      sidebar.hardwareWallets.length > 0
        ? {
            items: sidebar.hardwareWallets,
            activeWalletId: activeHardwareWalletId,
            actions: {
              onHardwareWalletItemClick: (walletId: string) => {
                console.debug('>>> ITEM CLICK: ', walletId);
                actions.sidebar.hardwareWalletSelected.trigger({ walletId });
              },
            },
          }
        : null;

    const sidebarMenus = {
      wallets: appWallets,
      hardwareWallets: isDev ? hardwareWallets : null,
    };

    const addWalletRoute = isHardwareWalletRoute ? ROUTES.HARDWARE_WALLETS.ADD : ROUTES.WALLETS.ADD

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
          actions.router.goToRoute.trigger({ route: addWalletRoute })
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

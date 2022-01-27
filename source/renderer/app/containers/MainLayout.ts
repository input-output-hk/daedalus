// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
import PaperWalletCreateCertificatePage from './wallet/PaperWalletCreateCertificatePage';
import InstructionsDialog from '../components/wallet/paper-wallet-certificate/InstructionsDialog';
import TransferFundsPage from './wallet/TransferFundsPage';
import AssetSettingsDialogContainer from './assets/AssetSettingsDialogContainer';
import type { InjectedContainerProps } from '../types/injectedPropsType';
import { ROUTES } from '../routes-config';
import type { WalletSortConfig } from '../types/sidebarTypes';

@inject('stores', 'actions')
@observer
export default class MainLayout extends Component<InjectedContainerProps> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  handleActivateCategory = (category: string) => {
    const { actions } = this.props;
    if (category === ROUTES.PAPER_WALLET_CREATE_CERTIFICATE) {
      actions.dialogs.open.trigger({ dialog: InstructionsDialog });
    } else if (category === ROUTES.NETWORK_INFO) {
      actions.networkStatus.toggleSplash.trigger();
    } else {
      actions.sidebar.activateSidebarCategory.trigger({ category });
    }
  };

  render() {
    const { actions, stores } = this.props;
    const {
      sidebar,
      profile,
      app,
      wallets: walletsStore,
      networkStatus,
    } = stores;
    const activeWallet = walletsStore.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
    const { isShelleyActivated } = networkStatus;
    const { currentTheme } = profile;
    const {
      environment: { network },
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
              onWalletSortBy: ({ sortBy }: WalletSortConfig) => {
                sidebar.onChangeWalletSortType(sortBy);
              },
              onSearch: sidebar.onSearchValueUpdated,
            },
            walletSortConfig: {
              sortBy: sidebar.walletSortConfig.sortBy,
              sortOrder: sidebar.walletSortConfig.sortOrder,
            },
            searchValue: sidebar.searchValue,
          }
        : null;

    const sidebarMenus = {
      wallets: appWallets,
    };

    const sidebarComponent = (
      <Sidebar
        menus={sidebarMenus}
        isShowingSubMenus={sidebar.isShowingSubMenus}
        isShelleyActivated={isShelleyActivated}
        categories={sidebar.CATEGORIES}
        activeSidebarCategory={sidebar.activeSidebarCategory}
        onActivateCategory={this.handleActivateCategory}
        onAddWallet={() =>
          actions.router.goToRoute.trigger({ route: ROUTES.WALLETS.ADD })
        }
        onSubmitSupportRequest={() =>
          actions.router.goToRoute.trigger({ route: ROUTES.SETTINGS.SUPPORT })
        }
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
          <AssetSettingsDialogContainer key="AssetSettingsDialogContainer" />,
        ]}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import Sidebar from '../components/sidebar/Sidebar';
import TopBarContainer from './TopBarContainer';
import SidebarLayout from '../components/layout/SidebarLayout';
import PaperWalletCreateCertificatePage from './wallet/PaperWalletCreateCertificatePage';
import InstructionsDialog from '../components/wallet/paper-wallet-certificate/InstructionsDialog';
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

  handleActivateCategory = (category: string) => {
    const { actions } = this.props;
    if (category === ROUTES.PAPER_WALLET_CREATE_CERTIFICATE) {
      actions.dialogs.open.trigger({ dialog: InstructionsDialog });
    } else if (category === ROUTES.REDEEM_ITN_REWARDS) {
      actions.staking.onRedeemStart.trigger();
    } else if (category === ROUTES.NETWORK_INFO) {
      actions.networkStatus.toggleSplash.trigger();
    } else {
      actions.sidebar.activateSidebarCategory.trigger({ category });
    }
  };

  render() {
    const { actions, stores } = this.props;
    const { sidebar, profile, app, wallets: walletsStore } = stores;
    const activeWallet = walletsStore.active;
    const activeWalletId = activeWallet ? activeWallet.id : null;
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
            activeWalletId,
            actions: {
              onHardwareWalletItemClick: (walletId: string) => {
                actions.sidebar.hardwareWalletSelected.trigger({ walletId });
              },
            },
          }
        : null;

    const sidebarMenus = {
      wallets: appWallets,
      hardwareWallets: isDev ? hardwareWallets : null,
    };

    const sidebarComponent = (
      <Sidebar
        menus={sidebarMenus}
        isShowingSubMenus={sidebar.isShowingSubMenus}
        isIncentivizedTestnet={global.isIncentivizedTestnet}
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
        ]}
      >
        {this.props.children}
      </SidebarLayout>
    );
  }
}

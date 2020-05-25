// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';
import HardwareWalletWithNavigation from '../../components/hardware-wallet/layouts/HardwareWalletWithNavigation';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class HardwareWallet extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  isActiveScreen = (page: string, item: NavDropdownProps) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
    const { options } = item;
    if (options && options.length) {
      options.forEach(option => {
        if (
          app.currentRoute &&
          app.currentRoute.includes(option.value.toString())
        ) {
          page = option.value.toString();
        }
      });
    }
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, {
      id: wallets.active.id,
      page,
    });
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores;
    if (!wallets.active) return;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.HARDWARE_WALLETS.PAGE,
      params: { id: wallets.active.id, page },
    });
  };

  render() {
    const { actions, stores } = this.props;
    const { app, wallets, walletSettings } = stores;
    const { restartNode } = actions.networkStatus;
    let { active: activeHardwareWallet } = wallets;

    const {
      isDeviceConnected,
      fetchingDevice,
      exportingExtendedPublicKey,
      isExportingPublicKeyAborted,
      isTrezor,
    } = wallets;
    const {
      hasNotification,
    } = walletSettings.getWalletsRecoveryPhraseVerificationData(
      activeHardwareWallet && activeHardwareWallet.id
        ? activeHardwareWallet.id
        : activeHardwareWallet
    );
    if (!activeHardwareWallet) {
      activeHardwareWallet = {
        walletNotConnected: true,
      };
    }
    const { walletNotConnected } = activeHardwareWallet;

    // @todo - remove after adding logic from store
    isLedger = true;

    return (
      <MainLayout>
        <HardwareWalletWithNavigation
          activeItem={app.currentPage}
          hasNotification={hasNotification}
          walletNotConnected={walletNotConnected}
          isDeviceConnected={isDeviceConnected}
          fetchingDevice={fetchingDevice}
          exportingExtendedPublicKey={exportingExtendedPublicKey}
          isExportingPublicKeyAborted={isExportingPublicKeyAborted}
          isLedger={isLedger}
          isTrezor={isTrezor}
          isActiveScreen={this.isActiveScreen}
          onOpenExternalLink={(url: string) => stores.app.openExternalLink(url)}
          onRestartNode={() => restartNode.trigger()}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          {this.props.children}
        </HardwareWalletWithNavigation>
      </MainLayout>
    );
  }
}

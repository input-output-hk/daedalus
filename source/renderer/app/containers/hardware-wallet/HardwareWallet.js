// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import MainLayout from '../MainLayout';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import HardwareWalletWithNavigation from '../../components/hardware-wallet/layouts/HardwareWalletWithNavigation';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class HardwareWallet extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  isActiveScreen = (page: string, item: NavDropdownProps) => {
    const { app, wallets } = this.props.stores;
    const { activeHardwareWallet } = wallets;
    if (!activeHardwareWallet) return false;
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
    const screenRoute = buildRoute(ROUTES.HARDWARE_WALLETS.PAGE, {
      id: activeHardwareWallet.id,
      page,
    });
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores;
    const { activeHardwareWallet } = wallets;
    if (!activeHardwareWallet) return;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.HARDWARE_WALLETS.PAGE,
      params: { id: activeHardwareWallet.id, page },
    });
  };

  render() {
    const { actions, stores } = this.props;
    const { app, wallets, walletSettings } = stores;
    const { restartNode } = actions.networkStatus;

    const { activeHardwareWallet } = wallets;
    const activeHardwareWalletId = get(activeHardwareWallet, 'id', null);

    const {
      isDeviceConnected,
      fetchingDevice,
      exportingExtendedPublicKey,
      isExportingPublicKeyAborted,
      isTrezor,
      availableHardwareWalletDevices,
    } = wallets;

    const {
      hasNotification,
    } = walletSettings.getWalletsRecoveryPhraseVerificationData(activeHardwareWalletId);
    const isWalletDisconnected = get(availableHardwareWalletDevices, [activeHardwareWalletId, 'disconnected'], true);

    // @todo - remove after adding logic from store
    const isLedger = true;

    return (
      <MainLayout>
        <HardwareWalletWithNavigation
          activeItem={app.currentPage}
          hasNotification={hasNotification}
          isWalletConnected={!isWalletDisconnected}
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

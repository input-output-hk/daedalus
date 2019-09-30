// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import { WalletSyncStateStatuses } from '../../domains/Wallet';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';
import { WalletRecoveryPhraseVerificationStatuses } from '../../stores/WalletsStore';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Wallet extends Component<Props> {
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
      route: ROUTES.WALLETS.PAGE,
      params: { id: wallets.active.id, page },
    });
  };

  render() {
    const { wallets, app } = this.props.stores;

    const { active: activeWallet } = wallets;

    if (!activeWallet)
      return (
        <MainLayout>
          <LoadingSpinner />
        </MainLayout>
      );

    const isRestoreActive =
      get(wallets, ['active', 'syncState', 'status']) === WalletSyncStateStatuses.RESTORING;
    const restoreProgress = get(
      activeWallet,
      ['syncState', 'progress', 'quantity'],
      0
    );

    const {
      recoveryPhraseVerificationStatus,
    } = wallets.getWalletRecoveryPhraseVerification(activeWallet.id);
    const hasNotification =
      recoveryPhraseVerificationStatus ===
      WalletRecoveryPhraseVerificationStatuses.NOTIFICATION;

    return (
      <MainLayout>
        {isRestoreActive ? (
          <RestoreNotification restoreProgress={restoreProgress} />
        ) : null}

        <WalletWithNavigation
          isActiveScreen={this.isActiveScreen}
          onWalletNavItemClick={this.handleWalletNavItemClick}
          activeItem={app.currentPage}
          hasNotification={hasNotification}
        >
          {this.props.children}
        </WalletWithNavigation>
      </MainLayout>
    );
  }
}

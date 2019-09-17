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
import { WalletSyncStateTags } from '../../domains/Wallet';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import type { NavDropdownProps } from '../../components/navigation/Navigation';

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
    const { wallets, profile, app } = this.props.stores;
    const { currentLocale } = profile;

    if (!wallets.active)
      return (
        <MainLayout>
          <LoadingSpinner />
        </MainLayout>
      );

    const isRestoreActive =
      get(wallets.active, 'syncState.tag') === WalletSyncStateTags.RESTORING;
    const restoreProgress = get(
      wallets.active,
      'syncState.data.percentage.quantity',
      0
    );
    const restoreETA = get(
      wallets.active,
      'syncState.data.estimatedCompletionTime.quantity',
      0
    );
    // TODO: Update info from the store #recovery
    const hasNotification = false;
    // const hasNotification =
    //   !!wallets.active &&
    //   wallets.active.recoveryPhraseVerificationStatus ===
    //     WalletStatuses.NOTIFICATION;

    return (
      <MainLayout>
        {isRestoreActive ? (
          <RestoreNotification
            currentLocale={currentLocale}
            restoreProgress={restoreProgress}
            restoreETA={restoreETA}
          />
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

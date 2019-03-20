// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import AdaRedemptionSuccessOverlay from '../../components/wallet/ada-redemption/AdaRedemptionSuccessOverlay';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import { WalletSyncStateTags } from '../../domains/Wallet';

type Props = InjectedContainerProps;

@inject('stores', 'actions')
@observer
export default class Wallet extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  constructor(props) {
    super(props);

    this.isSettingsPage = this.getIsSettingsPage;
  }

  isSettingsPage: boolean = false;

  isActiveScreen = (page: string) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, {
      id: wallets.active.id,
      page,
    });
    const currentRoute = app.currentRoute
      .replace('/general', '')
      .replace('/utxo', '');
    return currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores;
    if (!wallets.active) return;
    this.currentTab = page;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: wallets.active.id, page },
    });
    this.isSettingsPage = this.getIsSettingsPage;
  };

  get getIsSettingsPage() {
    return this.props.stores.app.currentRoute.indexOf('/settings') > -1;
  }

  render() {
    const { actions, stores } = this.props;
    const { wallets, adaRedemption, profile } = stores;
    const { showAdaRedemptionSuccessMessage, amountRedeemed } = adaRedemption;
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
          isSettingsPage={this.isSettingsPage}
        >
          {this.props.children}
        </WalletWithNavigation>

        {showAdaRedemptionSuccessMessage ? (
          <AdaRedemptionSuccessOverlay
            amount={amountRedeemed}
            onClose={
              actions.adaRedemption.closeAdaRedemptionSuccessOverlay.trigger
            }
          />
        ) : null}
      </MainLayout>
    );
  }
}

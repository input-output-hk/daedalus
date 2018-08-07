// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import { shell } from 'electron';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import AdaRedemptionSuccessOverlay from '../../components/wallet/ada-redemption/AdaRedemptionSuccessOverlay';
import RestoreNotification from '../../components/notifications/RestoreNotification';
import { buildRoute } from '../../utils/routing';
import { ROUTES } from '../../routes-config';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import { syncStateTags } from '../../domains/Wallet';
import environment from '../../../../common/environment';
import AntivirusRestaurationSlowdownNotification
  from '../../components/notifications/AntivirusRestaurationSlowdownNotification';

type Props = InjectedContainerProps;

@inject('stores', 'actions') @observer
export default class Wallet extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  isActiveScreen = (page: string) => {
    const { app } = this.props.stores;
    const { wallets } = this.props.stores.ada;
    if (!wallets.active) return false;
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, { id: wallets.active.id, page });
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores.ada;
    if (!wallets.active) return;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: wallets.active.id, page },
    });
  };

  handleAntivirusNotificationDiscard = () => {
    const { wallets } = this.props.actions.ada;
    wallets.discardAntivirusRestorationSlowdownNotificationForActiveWallet.trigger();
  };

  openExternalLinkInDefaultBrowser = (event: MouseEvent) => {
    event.preventDefault();
    if (event.target.href) shell.openExternal(event.target.href);
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets, adaRedemption } = stores.ada;
    const { showAdaRedemptionSuccessMessage, amountRedeemed } = adaRedemption;
    const { currentLocale } = stores.profile;

    if (!wallets.active) return <MainLayout><LoadingSpinner /></MainLayout>;

    const isRestoreActive = get(wallets.active, 'syncState.tag') === syncStateTags.RESTORING;
    const restoreProgress = get(wallets.active, 'syncState.data.percentage.quantity', 0);
    const restoreETA = get(wallets.active, 'syncState.data.estimatedCompletionTime.quantity', 0);

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
        >
          {this.props.children}
        </WalletWithNavigation>

        {showAdaRedemptionSuccessMessage ? (
          <AdaRedemptionSuccessOverlay
            amount={amountRedeemed}
            onClose={actions.ada.adaRedemption.closeAdaRedemptionSuccessOverlay.trigger}
          />
        ) : null}
        {
          environment.isWindows() &&
          isRestoreActive &&
          !wallets.hasDiscardedAntivirusRestorationSlowdownNotificationForActiveWallet ? (
          <AntivirusRestaurationSlowdownNotification
            onDiscard={this.handleAntivirusNotificationDiscard}
            onFaqLinkClick={this.openExternalLinkInDefaultBrowser}
          />
        ) : null}
      </MainLayout>
    );
  }
}

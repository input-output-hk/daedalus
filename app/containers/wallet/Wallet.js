// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import AdaRedemptionSuccessOverlay from '../../components/wallet/ada-redemption/AdaRedemptionSuccessOverlay';
import { buildRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../Routes';
import type { InjectedContainerProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class Wallet extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedContainerProps;

  isActiveScreen = (page: string) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, { id: wallets.active.id, page });
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
    const { wallets, adaRedemption } = this.props.stores;
    const { actions } = this.props;
    const { showAdaRedemptionSuccessMessage, amountRedeemed } = adaRedemption;
    if (!wallets.active) return <MainLayout><LoadingSpinner /></MainLayout>;
    return (
      <MainLayout>
        <WalletWithNavigation
          isActiveScreen={this.isActiveScreen}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          {this.props.children}
        </WalletWithNavigation>
        {showAdaRedemptionSuccessMessage ? (
          <AdaRedemptionSuccessOverlay
            amount={amountRedeemed}
            onClose={actions.adaRedemption.closeAdaRedemptionSuccessOverlay}
          />
        ) : null}
      </MainLayout>
    );
  }
}

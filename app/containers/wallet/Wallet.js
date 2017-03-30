// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import MainLayout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';
import { oneOrManyChildElements } from '../../propTypes';
import AdaRedemptionSuccessOverlay from '../../components/wallet/ada-redemption/AdaRedemptionSuccessOverlay';
import { buildRoute } from '../../lib/routing-helpers';
import { ROUTES } from '../../Routes';

@inject('stores', 'actions') @observer
export default class Wallet extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      app: PropTypes.shape({
        currentRoute: PropTypes.string.isRequired
      }).isRequired,
      wallets: PropTypes.shape({
        hasLoadedWallets: PropTypes.bool.isRequired
      }).isRequired,
      adaRedemption: PropTypes.shape({
        showAdaRedemptionSuccessMessage: PropTypes.bool.isRequired,
        amountRedeemed: PropTypes.number.isRequired,
      }),
    }).isRequired,
    actions: PropTypes.shape({
      router: PropTypes.shape({
        goToRoute: PropTypes.func.isRequired,
      }),
      adaRedemption: PropTypes.shape({
        closeAdaRedemptionSuccessOverlay: PropTypes.func.isRequired,
      }),
    }).isRequired,
    children: oneOrManyChildElements,
  };

  isActiveScreen = (page: string) => {
    const { app, wallets } = this.props.stores;
    if (!wallets.active) return false;
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, { id: wallets.active.id, page });
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores;
    this.props.actions.router.goToRoute({
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

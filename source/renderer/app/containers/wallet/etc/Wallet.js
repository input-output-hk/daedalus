// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletWithNavigation from '../../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../../components/widgets/LoadingSpinner';
import resolver from '../../../utils/imports';
import { buildRoute } from '../../../utils/routing';
import { ROUTES } from '../../../routes-config';
import type { InjectedContainerProps } from '../../../types/injectedPropsType';

const MainLayout = resolver('containers/MainLayout');

type Props = InjectedContainerProps;

@inject('stores', 'actions') @observer
export default class Wallet extends Component<Props> {

  static defaultProps = { stores: null };

  isActiveScreen = (page: string) => {
    const { app } = this.props.stores;
    const { wallets } = this.props.stores.etc;
    if (!wallets.active) return false;
    const screenRoute = buildRoute(ROUTES.WALLETS.PAGE, { id: wallets.active.id, page });
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (page: string) => {
    const { wallets } = this.props.stores.etc;
    if (!wallets.active) return;
    this.props.actions.router.goToRoute.trigger({
      route: ROUTES.WALLETS.PAGE,
      params: { id: wallets.active.id, page },
    });
  };

  render() {
    const { wallets } = this.props.stores.etc;
    if (!wallets.active) return <MainLayout><LoadingSpinner /></MainLayout>;
    return (
      <MainLayout>
        <WalletWithNavigation
          isActiveScreen={this.isActiveScreen}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          {this.props.children}
        </WalletWithNavigation>
      </MainLayout>
    );
  }
}

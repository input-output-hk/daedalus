// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import LoadingSpinner from '../../components/widgets/LoadingSpinner';

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
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
    }).isRequired,
  };

  isActiveScreen = (screen: string) => {
    const { app, wallets} = this.props.stores;
    if (!wallets.active) return false;
    const screenRoute = `${wallets.BASE_ROUTE}/${wallets.active.id}/${screen}`;
    return app.currentRoute === screenRoute;
  };

  handleWalletNavItemClick = (item: string) => {
    const { wallets } = this.props.stores;
    this.props.actions.goToRoute({ route: `${wallets.BASE_ROUTE}/${wallets.active.id}/${item}` });
  };

  render() {
    const { wallets } = this.props.stores;
    if (!wallets.active) return <Layout><LoadingSpinner /></Layout>;
    return (
      <Layout>
        <WalletWithNavigation
          wallet={wallets.active}
          isActiveScreen={this.isActiveScreen}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          {this.props.children}
        </WalletWithNavigation>
      </Layout>
    );
  }
}

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
      router: PropTypes.shape({
        location: PropTypes.shape({ pathname: PropTypes.string.isRequired })
      }).isRequired
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
    }).isRequired,
  };

  isActiveScreen = (screen: string) => {
    const { router, wallets} = this.props.stores;
    if (!wallets.active) return false;
    const screenRoute = `${wallets.BASE_ROUTE}/${wallets.active.id}/${screen}`;
    return router.location ? router.location.pathname === screenRoute : false;
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

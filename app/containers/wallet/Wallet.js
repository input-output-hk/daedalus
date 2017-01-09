// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';

@inject('stores', 'actions') @observer
export default class Wallet extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      routing: PropTypes.shape({
        location: PropTypes.shape({ pathname: PropTypes.string.isRequired })
      }).isRequired
    }).isRequired,
    actions: PropTypes.shape({
      goToRoute: PropTypes.func.isRequired,
    }).isRequired,
    pathname: PropTypes.string.isRequired,
  };

  isActiveScreen = (screen: string) => {
    const { routing, wallets} = this.props.stores;
    const screenRoute = `${wallets.BASE_ROUTE}/${wallets.active.id}/${screen}`;
    return routing.location ? routing.location.pathname === screenRoute : false;
  };

  handleWalletNavItemClick = (item: string) => {
    const { wallets } = this.props.stores;
    this.props.actions.goToRoute({ route: `${wallets.BASE_ROUTE}/${wallets.active.id}/${item}` });
  };

  render() {
    const { pathname } = this.props;
    const { wallets } = this.props.stores;
    const { BASE_ROUTE } = wallets;
    return (
      <Layout>
        <WalletWithNavigation
          wallet={wallets.active}
          isActiveScreen={this.isActiveScreen}
          onWalletNavItemClick={this.handleWalletNavItemClick}
        >
          <Match pattern={`${BASE_ROUTE}/:id`} render={() => <Redirect to={`${pathname}/home`} />} />
          <Match pattern={`${BASE_ROUTE}/:id/home`} component={WalletHomePage} />
          <Match pattern={`${BASE_ROUTE}/:id/send`} component={WalletSendPage} />
          <Match pattern={`${BASE_ROUTE}/:id/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      </Layout>
    );
  }
}

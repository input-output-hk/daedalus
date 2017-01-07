// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, inject } from 'mobx-react';
import Layout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';
import AppController from '../../controllers/AppController';
import { appStatePropType } from '../../state/index';

@inject('stores', 'controller') @observer
export default class Wallet extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      routing: PropTypes.shape({
        location: PropTypes.shape({ pathname: PropTypes.string.isRequired })
      }).isRequired
    }).isRequired,
    controller: PropTypes.instanceOf(AppController).isRequired,
    pathname: PropTypes.string.isRequired,
  };

  static WALLET_BASE_PATH = '/wallet';

  isActiveScreen(screen: string) {
    const { routing, wallets} = this.props.stores;
    const screenRoute = `${Wallet.WALLET_BASE_PATH}/${wallets.active.id}/${screen}`;
    return routing.location ? routing.location.pathname === screenRoute : false;
  }

  handleWalletNavItemClick(item: string) {
    const { wallets } = this.props.stores;
    this.props.controller.navigateTo(`${Wallet.WALLET_BASE_PATH}/${wallets.active.id}/${item}`);
  }

  render() {
    const { pathname } = this.props;
    const { wallets } = this.props.stores;
    const WALLET_BASE_PATH = Wallet.WALLET_BASE_PATH;
    return (
      <Layout>
        <WalletWithNavigation
          wallet={wallets.active}
          isActiveScreen={this.isActiveScreen.bind(this)}
          onWalletNavItemClick={this.handleWalletNavItemClick.bind(this)}
        >
          <Match pattern={`${WALLET_BASE_PATH}/:id`} render={() => <Redirect to={`${pathname}/home`} />} />
          <Match pattern={`${WALLET_BASE_PATH}/:id/home`} component={WalletHomePage} />
          <Match pattern={`${WALLET_BASE_PATH}/:id/send`} component={WalletSendPage} />
          <Match pattern={`${WALLET_BASE_PATH}/:id/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      </Layout>
    );
  }
}

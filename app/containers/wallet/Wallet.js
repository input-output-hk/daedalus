// @flow
import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
import Layout from '../MainLayout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';
import AppController from '../../controllers/AppController';
import { appStatePropType } from '../../state/index';

@observer(['state', 'controller'])
export default class Wallet extends Component {

  static propTypes = {
    state: appStatePropType.isRequired,
    controller: PropTypes.instanceOf(AppController).isRequired,
    pathname: PropTypes.string.isRequired
  };

  static walletPath = '/wallet';

  isActiveScreen(screen: string) {
    const { router, activeWallet } = this.props.state;
    if (router.location) {
      return router.location.pathname === `${Wallet.walletPath}/${activeWallet.wallet.id}/${screen}`;
    }
    return false;
  }

  handleWalletNavItemClick(item: string) {
    const { activeWallet } = this.props.state;
    this.props.controller.navigateTo(`${Wallet.walletPath}/${activeWallet.wallet.id}/${item}`);
  }

  render() {
    const { pathname } = this.props;
    const { activeWallet } = this.props.state;
    const { wallet } = activeWallet;
    const walletPath = Wallet.walletPath;
    return (
      <Layout>
        <WalletWithNavigation
          wallet={wallet}
          isActiveScreen={this.isActiveScreen.bind(this)}
          onWalletNavItemClick={this.handleWalletNavItemClick.bind(this)}
        >
          <Match pattern={`${walletPath}/:id`} render={() => <Redirect to={`${pathname}/home`} />} />
          <Match pattern={`${walletPath}/:id/home`} component={WalletHomePage} />
          <Match pattern={`${walletPath}/:id/send`} component={WalletSendPage} />
          <Match pattern={`${walletPath}/:id/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      </Layout>
    );
  }
}

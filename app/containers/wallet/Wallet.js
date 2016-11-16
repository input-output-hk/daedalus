import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Layout from '../Layout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';

@observer(['state', 'controller'])
export default class Wallet extends Component {

  static propTypes = {
    state: PropTypes.shape({
      activeWallet: PropTypes.shape({
        wallet: MobxPropTypes.observableObject,
      })
    }),
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        switchWallet: PropTypes.func.isRequired,
      })
    }),
    pathname: PropTypes.string.isRequired,
    params: PropTypes.shape({
      id: PropTypes.string.isRequired
    })
  };

  render() {
    const { activeWallet } = this.props.state;
    const { wallet } = activeWallet;
    const walletPath = this.props.pathname;
    const { params } = this.props;
    if (params.id !== wallet.address) {
      this.props.controller.wallets.switchWallet(params.id);
    }
    return (
      <Layout>
        <WalletWithNavigation wallet={wallet}>
          <Match pattern={`${walletPath}/${wallet.address}/create`} render={() => <Redirect to={`${walletPath}/${wallet.address}/home`} />} />
          <Match pattern={`${walletPath}/${wallet.address}/home`} component={WalletHomePage} />
          <Match pattern={`${walletPath}/${wallet.address}/send`} component={WalletSendPage} />
          <Match pattern={`${walletPath}/${wallet.address}/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      </Layout>
    );
  }
}

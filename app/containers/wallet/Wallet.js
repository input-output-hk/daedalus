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
    pathname: PropTypes.string.isRequired,
  };

  render() {
    const { activeWallet } = this.props.state;
    const { wallet } = activeWallet;
    const walletPath = '/wallet';
    return (
      <Layout>
        <WalletWithNavigation wallet={wallet}>
          <Match pattern={`${walletPath}/:id`} render={() => <Redirect to={`${walletPath}/home`} />} />
          <Match pattern={`${walletPath}/:id/home`} component={WalletHomePage} />
          <Match pattern={`${walletPath}/:id/send`} component={WalletSendPage} />
          <Match pattern={`${walletPath}/:id/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      </Layout>
    );
  }
}

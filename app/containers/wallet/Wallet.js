import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import Layout from '../Layout';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';
import WalletCreatePage from './WalletCreatePage';

@observer(['selectedWallet'])
export default class Wallet extends Component {

  static propTypes = {
    selectedWallet: MobxPropTypes.observableObject,
    pathname: PropTypes.string.isRequired
  };

  render() {
    const { selectedWallet } = this.props;
    const walletPath = this.props.pathname;
    let walletPage = null;
    // Redirect from/to wallet create screen if there is none yet
    if (selectedWallet) {
      walletPage = (
        <Layout>
          <WalletWithNavigation wallet={selectedWallet}>
            <Match pattern={`${walletPath}/create`} render={() => <Redirect to={`${walletPath}/home`} />} />
            <Match pattern={`${walletPath}/home`} component={WalletHomePage} />
            <Match pattern={`${walletPath}/send`} component={WalletSendPage} />
            <Match pattern={`${walletPath}/receive`} component={WalletReceivePage} />
          </WalletWithNavigation>
        </Layout>
      );
    } else {
      walletPage = (
        <div style={{ height: '100%' }}>
          <Match pattern={walletPath} render={() => <Redirect to={`${walletPath}/create`} />} />
          <Match pattern={`${walletPath}/create`} component={WalletCreatePage} />
        </div>
      );
    }
    return walletPage;
  }
}

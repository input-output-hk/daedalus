import React, { Component, PropTypes } from 'react';
import { Match, Redirect } from 'react-router';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletHomePage from './WalletHomePage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';
import WalletCreatePage from './WalletCreatePage';

@observer(['store'])
export default class Wallet extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object
    }),
    pathname: PropTypes.string.isRequired
  };

  render() {
    const { wallet } = this.props.store;
    const { pathname } = this.props;
    let walletPages = null;

    if (wallet) {
      walletPages = (
        <WalletWithNavigation wallet={wallet}>
          <Match pattern={`${pathname}/home`} component={WalletHomePage} />
          <Match pattern={`${pathname}/send`} component={WalletSendPage} />
          <Match pattern={`${pathname}/receive`} component={WalletReceivePage} />
        </WalletWithNavigation>
      );
    } else {
      walletPages = (
        <Match pattern={`${pathname}/create`} component={WalletCreatePage} />
      );
    }

    return (
      <div style={{ height: '100%' }}>
        <Match
          pattern={pathname}
          exactly
          render={() => {
            if (wallet) {
              return <Redirect to={`${pathname}/home`} />;
            }
            return <Redirect to={`${pathname}/create`} />;
          }}
        />
        {walletPages}
      </div>
    );
  }
}

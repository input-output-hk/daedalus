import React, { Component, PropTypes } from 'react';
import { Match } from 'react-router';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
import WalletDetailsPage from './WalletDetailsPage';
import WalletReceivePage from './WalletReceivePage';
import WalletSendPage from './WalletSendPage';

@observer(['store'])
export default class Wallet extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired
    }),
    pathname: PropTypes.string.isRequired
  };

  render() {
    const { wallet } = this.props.store;
    const { pathname } = this.props;
    return (
      <WalletWithNavigation wallet={wallet}>
        <div>
          <Match exactly pattern={pathname} component={WalletDetailsPage} />
          <Match pattern={`${pathname}/send`} component={WalletSendPage} />
          <Match pattern={`${pathname}/receive`} component={WalletReceivePage} />
        </div>
      </WalletWithNavigation>
    );
  }

}

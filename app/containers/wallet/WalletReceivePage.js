// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/layouts/WalletWithNavigation';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletSendPage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired,
      walletReceive: PropTypes.object.isRequired
    })
  };

  render() {
    const { wallet, walletReceive } = this.props.store;
    return (
      <WalletWithNavigation wallet={wallet}>
        <WalletReceive walletReceive={walletReceive} />
      </WalletWithNavigation>
    );
  }

}

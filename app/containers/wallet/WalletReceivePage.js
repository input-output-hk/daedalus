// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';
import Wallet from '../../domain/Wallet';

@inject('stores') @observer
export default class WalletReceivePage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet).isRequired
      }).isRequired
    }).isRequired
  };

  render() {
    const wallet = this.props.stores.wallets.active;
    return (
      <WalletReceive walletName={wallet.name} walletAddress={wallet.address} />
    );
  }

}

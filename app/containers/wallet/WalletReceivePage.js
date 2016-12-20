// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@inject('state') @observer
export default class WalletReceivePage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      activeWallet: PropTypes.shape({
        wallet: MobxPropTypes.observableObject.isRequired
      })
    })
  };

  render() {
    const { wallet } = this.props.state.activeWallet;
    return (
      <WalletReceive walletName={wallet.name} walletAddress={wallet.address} />
    );
  }

}

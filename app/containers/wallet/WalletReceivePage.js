// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletReceivePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallets: PropTypes.shape({
        activeWallet: MobxPropTypes.observableObject.isRequired
      })
    })
  };

  render() {
    const { activeWallet } = this.props.store.wallets;
    return (
      <WalletReceive walletName={activeWallet.name} walletAddress={activeWallet.address} />
    );
  }

}

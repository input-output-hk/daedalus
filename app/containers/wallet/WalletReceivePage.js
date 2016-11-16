// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletReceivePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      activeWallet: PropTypes.shape({
        wallet: MobxPropTypes.observableObject.isRequired
      })
    })
  };

  render() {
    const { wallet } = this.props.store.activeWallet;
    return (
      <WalletReceive walletName={wallet.name} walletAddress={wallet.address} />
    );
  }

}

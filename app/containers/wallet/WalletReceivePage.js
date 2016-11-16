// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['state'])
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

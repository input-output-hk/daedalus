// @flow
import React, { Component } from 'react';
import { observer, PropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['selectedWallet'])
export default class WalletReceivePage extends Component {

  static propTypes = {
    selectedWallet: PropTypes.observableObject.isRequired,
  };

  render() {
    const { selectedWallet } = this.props;
    return (
      <WalletReceive walletName={selectedWallet.name} walletAddress={selectedWallet.address} />
    );
  }

}

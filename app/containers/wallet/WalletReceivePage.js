// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['state'])
export default class WalletReceivePage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      uiStore: PropTypes.shape({
        selectedWallet: PropTypes.object.isRequired,
      })
    })
  };

  render() {
    const { selectedWallet } = this.props.state.uiStore;
    return (
      <WalletReceive walletName={selectedWallet.name} walletAddress={selectedWallet.address} />
    );
  }

}

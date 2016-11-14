// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletReceivePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      selectedWallet: MobxPropTypes.observableObject.isRequired,
    })
  };

  render() {
    const { selectedWallet } = this.props.store;
    return (
      <WalletReceive walletName={selectedWallet.name} walletAddress={selectedWallet.address} />
    );
  }

}

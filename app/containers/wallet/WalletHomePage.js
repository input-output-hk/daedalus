// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['store'])
export default class WalletHomePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      uiStore: PropTypes.shape({
        selectedWallet: PropTypes.object.isRequired
      })
    })
  };

  render() {
    const { selectedWallet } = this.props.store.uiStore;
    return (
      <WalletHome transactions={selectedWallet.transactions} />
    );
  }

}

// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['state'])
export default class WalletHomePage extends Component {

  static propTypes = {
    state: PropTypes.shape({
      uiStore: PropTypes.shape({
        selectedWallet: PropTypes.object.isRequired
      })
    })
  };

  render() {
    const { selectedWallet } = this.props.state.uiStore;
    return (
      <WalletHome transactions={selectedWallet.transactions} />
    );
  }

}

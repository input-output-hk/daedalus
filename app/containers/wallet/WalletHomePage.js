// @flow
import React, { Component } from 'react';
import { observer, PropTypes } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['selectedWallet'])
export default class WalletHomePage extends Component {

  static propTypes = {
    selectedWallet: PropTypes.observableObject.isRequired
  };

  render() {
    const { selectedWallet } = this.props;
    return (
      <WalletHome transactions={selectedWallet.transactions} />
    );
  }

}

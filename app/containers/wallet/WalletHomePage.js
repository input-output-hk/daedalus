// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['store'])
export default class WalletHomePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired,
    })
  };

  render() {
    const { wallet } = this.props.store;
    return (
      <WalletHome transactions={wallet.transactions} />
    );
  }

}

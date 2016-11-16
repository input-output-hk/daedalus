// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletTransactionsList from '../../components/wallet/home/WalletTransactionsList';

@observer(['state'])
export default class WalletHomePage extends Component {

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
      <WalletTransactionsList transactions={wallet.transactions} />
    );
  }

}

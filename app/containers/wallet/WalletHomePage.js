// @flow
import React, { Component, PropTypes } from 'react';
import { observer, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletHome from '../../components/wallet/WalletHome';

@observer(['store'])
export default class WalletHomePage extends Component {

  static propTypes = {
    store: PropTypes.shape({
      wallets: PropTypes.shape({
        activeWallet: MobxPropTypes.observableObject.isRequired
      })
    })
  };

  render() {
    const { activeWallet } = this.props.store.wallets;
    return (
      <WalletHome transactions={activeWallet.transactions} />
    );
  }

}

// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/wallet/layouts/WalletWithNavigation';
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
      <WalletWithNavigation wallet={wallet}>
        <WalletHome transactions={wallet.transactions} />
      </WalletWithNavigation>
    );
  }

}

// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletWithNavigation from '../../components/layouts/WalletWithNavigation';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletSendPage extends Component {
  static propTypes = {
    store: PropTypes.shape({
      wallet: PropTypes.object.isRequired,
      walletReceive: PropTypes.object.isRequired
    })
  };
  render() {
    return (
      <WalletWithNavigation wallet={this.props.store.wallet}>
        <WalletReceive walletReceive={this.props.store.walletReceive} />
      </WalletWithNavigation>
    );
  }
}

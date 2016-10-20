// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import WalletLayout from '../../components/layouts/WalletLayout';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletSendPage extends Component {
  static propTypes = {
    store: PropTypes.shape({
      walletReceive: PropTypes.object.isRequired
    })
  };
  render() {
    return (
      <WalletLayout>
        <WalletReceive walletReceive={this.props.store.walletReceive} />
      </WalletLayout>
    );
  }
}

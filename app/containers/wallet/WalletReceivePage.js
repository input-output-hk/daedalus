// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletLayout from '../../components/layouts/WalletLayout';
import WalletReceive from '../../components/wallet/WalletReceive';

@observer(['store'])
export default class WalletSendPage extends Component {
  render() {
    return (
      <WalletLayout>
        <WalletReceive />
      </WalletLayout>
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import walletSendFormValidator from '../../validators/walletSendFormValidator';

@observer
export default class WalletSendPage extends Component {

  render() {
    return (
      <WalletSendForm validator={walletSendFormValidator} />
    );
  }

}

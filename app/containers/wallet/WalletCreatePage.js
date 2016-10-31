// @flow
import React, { Component } from 'react';
import WalletCreateForm from '../../components/wallet/WalletCreateForm';
import walletCreateFormValidator from '../../validators/walletCreateFormValidator';

export default class WalletCreatePage extends Component {

  render() {
    return (
      <WalletCreateForm validator={walletCreateFormValidator} />
    );
  }

}

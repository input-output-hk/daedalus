// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletCreateForm from '../../components/wallet/WalletCreateForm';
import walletCreateFormValidator from '../../validators/walletCreateFormValidator';

@observer
export default class WalletCreatePage extends Component {

  render() {
    return (
      <WalletCreateForm validator={walletCreateFormValidator} />
    );
  }

}

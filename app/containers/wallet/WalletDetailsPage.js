// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletDetails from '../../components/wallet/WalletDetails';

@observer(['store'])
export default class WalletDetailsPage extends Component {

  render() {
    return (
      <WalletDetails />
    );
  }

}

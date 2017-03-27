// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletSettings from '../../components/wallet/WalletSettings';

@observer
export default class WalletSettingsPage extends Component {

  render() {
    return (
      <WalletSettings />
    );
  }

}

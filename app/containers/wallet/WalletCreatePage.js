// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';
import AppController from '../../controllers/AppController';

@inject('controller') @observer
export default class WalletCreatePage extends Component {

  static propTypes = {
    controller: PropTypes.instanceOf(AppController).isRequired,
  };

  createPersonalWallet(values) {
    this.props.controller.wallets.createPersonalWallet({
      name: values.walletName,
      currency: values.currency,
    });
  }

  render() {
    return (
      <CenteredLayout>
        <WalletCreateDialog onSubmit={this.createPersonalWallet.bind(this)} />
      </CenteredLayout>
    );
  }
}

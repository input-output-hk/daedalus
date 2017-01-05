// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';

@inject('controller') @observer
export default class WalletSendPage extends Component {

  static propTypes = {
    controller: PropTypes.shape({
      wallets: PropTypes.shape({
        isValidAddress: PropTypes.func.isRequired,
        sendMoney: PropTypes.func.isRequired,
      })
    }),
  };

  handleWalletSendFormSubmit(values: Object) {
    this.props.controller.wallets.sendMoney(values);
  }

  render() {
    return (
      <WalletSendForm
        onSubmit={this.handleWalletSendFormSubmit.bind(this)}
        addressValidator={address => this.props.controller.wallets.isValidAddress(address)}
      />
    );
  }

}

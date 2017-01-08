// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        isValidAddress: PropTypes.func.isRequired,
      })
    }),
    actions: PropTypes.shape({
      sendMoney: PropTypes.func.isRequired,
    }),
  };

  handleWalletSendFormSubmit(values: Object) {
    this.props.actions.sendMoney(values);
  }

  render() {
    return (
      <WalletSendForm
        onSubmit={this.handleWalletSendFormSubmit.bind(this)}
        addressValidator={address => this.props.stores.wallets.isValidAddress(address)}
      />
    );
  }

}

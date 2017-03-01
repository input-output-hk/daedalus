// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import Request from '../../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        isValidAddress: PropTypes.func.isRequired,
        sendMoneyRequest: PropTypes.instanceOf(Request),
      })
    }),
    actions: PropTypes.shape({
      wallets: PropTypes.shape({
        sendMoney: PropTypes.func.isRequired,
      }),
    }),
  };

  handleWalletSendFormSubmit(values: Object) {
    this.props.actions.wallets.sendMoney(values);
  }

  render() {
    const { isValidAddress, sendMoneyRequest } = this.props.stores.wallets;
    return (
      <WalletSendForm
        onSubmit={this.handleWalletSendFormSubmit.bind(this)}
        isSubmitting={sendMoneyRequest.isExecuting}
        addressValidator={address => isValidAddress(address)}
        error={sendMoneyRequest.error}
      />
    );
  }

}

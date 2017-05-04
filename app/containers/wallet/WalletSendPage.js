// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  handleWalletSendFormSubmit(values: Object) {
    this.props.actions.wallets.sendMoney.trigger(values);
  }

  render() {
    const { wallets } = this.props.stores;
    const { isValidAddress, sendMoneyRequest } = wallets;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendForm
        onSubmit={this.handleWalletSendFormSubmit.bind(this)}
        isSubmitting={sendMoneyRequest.isExecuting}
        addressValidator={address => isValidAddress(address)}
        isWalletPasswordSet={activeWallet.hasPassword}
        error={sendMoneyRequest.error}
      />
    );
  }

}

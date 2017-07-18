// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  handleWalletSendFormSubmit = (values: Object) => {
    this.props.actions.wallets.sendMoney.trigger(values);
  };

  render() {
    const { wallets, transactions } = this.props.stores;
    const { isValidAddress, sendMoneyRequest } = wallets;
    const { calculateTransactionFee } = transactions;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendForm
        onSubmit={this.handleWalletSendFormSubmit}
        calculateTransactionFee={(to, amount) => (
          calculateTransactionFee(activeWallet.address, to, amount)
        )}
        isSubmitting={sendMoneyRequest.isExecuting}
        addressValidator={isValidAddress}
        isWalletPasswordSet={activeWallet.hasPassword}
        error={sendMoneyRequest.error}
      />
    );
  }

}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSendForm from '../../../components/wallet/WalletSendForm';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSendPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const { uiDialogs } = this.props.stores;
    const { /* transactions, */ wallets } = this.props.stores.etc;
    const { actions } = this.props;
    // const { isValidAddress } = wallets;
    // const { calculateTransactionFee } = transactions;
    const activeWallet = wallets.active;

    // Faked missing data and methods
    const isValidAddress = () => (true);
    const calculateTransactionFee = () => ('0');

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendForm
        calculateTransactionFee={(receiver, amount) => (
          calculateTransactionFee(activeWallet.id, receiver, amount)
        )}
        addressValidator={isValidAddress}
        isDialogOpen={uiDialogs.isOpen}
        openDialogAction={actions.dialogs.open.trigger}
      />
    );
  }

}

// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import WalletSendConfirmationDialog from '../../../components/wallet/WalletSendConfirmationDialog';

@inject('actions', 'stores') @observer
export default class WalletSendConfirmationDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null };

  props: {
    stores: any | StoresMap,
    actions: any | ActionsMap,
    amount: string,
    receiver: string,
    totalAmount: string,
    transactionFee: string,
    adaToLovelaces: Function,
  };

  handleWalletSendFormSubmit = (values: Object) => {
    this.props.actions.wallets.sendMoney.trigger(values);
  };

  render() {
    const { actions, amount, receiver, totalAmount, transactionFee, adaToLovelaces } = this.props;
    const { wallets } = this.props.stores;
    const { sendMoneyRequest } = wallets;
    const activeWallet = wallets.active;

    if (!activeWallet) throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendConfirmationDialog
        isWalletPasswordSet={activeWallet.hasPassword}
        amount={amount}
        receiver={receiver}
        totalAmount={totalAmount}
        transactionFee={transactionFee}
        adaToLovelaces={adaToLovelaces}
        onSubmit={this.handleWalletSendFormSubmit}
        isSubmitting={sendMoneyRequest.isExecuting}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
        error={sendMoneyRequest.error}
      />
    );
  }

}

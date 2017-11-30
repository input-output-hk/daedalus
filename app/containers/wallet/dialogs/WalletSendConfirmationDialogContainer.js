// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import environment from '../../../environment';
import resolver from '../../../utils/imports';

const WalletSendConfirmationDialog = resolver('components/wallet/WalletSendConfirmationDialog');

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  amount: string,
  receiver: string,
  totalAmount: string,
  transactionFee: string,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  currencyUnit: string,
};

@inject('actions', 'stores') @observer
export default class WalletSendConfirmationDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  handleWalletSendFormSubmit = (values: Object) => {
    this.props.actions[environment.API].wallets.sendMoney.trigger(values);
  };

  render() {
    const {
      actions, amount, receiver, totalAmount,
      transactionFee, amountToNaturalUnits, currencyUnit
    } = this.props;
    const { wallets } = this.props.stores[environment.API];
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
        amountToNaturalUnits={amountToNaturalUnits}
        onSubmit={this.handleWalletSendFormSubmit}
        isSubmitting={sendMoneyRequest.isExecuting}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          sendMoneyRequest.reset();
        }}
        error={sendMoneyRequest.error}
        currencyUnit={currencyUnit}
      />
    );
  }

}

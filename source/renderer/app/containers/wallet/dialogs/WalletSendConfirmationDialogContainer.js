// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import WalletSendConfirmationDialog from '../../../components/wallet/WalletSendConfirmationDialog';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  amount: string,
  receiver: string,
  totalAmount: ?string,
  transactionFee: ?string,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  currencyUnit: string,
  onExternalLinkClick: Function,
};

@inject('actions', 'stores')
@observer
export default class WalletSendConfirmationDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleWalletSendFormSubmit = (values: Object) => {
    this.props.actions.wallets.sendMoney.trigger(values);
  };

  render() {
    const {
      actions,
      amount,
      receiver,
      totalAmount,
      onExternalLinkClick,
      transactionFee,
      amountToNaturalUnits,
      currencyUnit,
    } = this.props;
    const { stores } = this.props;
    const { sendMoneyRequest, active: activeWallet } = stores.wallets;
    const { isMainnet } = stores.app.environment;

    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    return (
      <WalletSendConfirmationDialog
        amount={amount}
        receiver={receiver}
        totalAmount={totalAmount}
        transactionFee={transactionFee}
        amountToNaturalUnits={amountToNaturalUnits}
        onSubmit={this.handleWalletSendFormSubmit}
        isSubmitting={sendMoneyRequest.isExecuting}
        isMainnet={isMainnet}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          sendMoneyRequest.reset();
        }}
        error={sendMoneyRequest.error}
        currencyUnit={currencyUnit}
        onExternalLinkClick={onExternalLinkClick}
      />
    );
  }
}

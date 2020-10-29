// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { StoresMap } from '../../../stores/index';
import type { ActionsMap } from '../../../actions/index';
import type { HwDeviceStatus } from '../../../domains/Wallet';
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
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
};

@inject('actions', 'stores')
@observer
export default class WalletSendConfirmationDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleWalletSendFormSubmit = (values: Object) => {
    if (values.isHardwareWallet) {
      this.props.actions.hardwareWallets.sendMoney.trigger({});
    } else {
      this.props.actions.wallets.sendMoney.trigger(values);
    }
  };

  handleInitiateTransaction = () => {
    const { stores } = this.props;
    const { wallets, hardwareWallets } = stores;
    const { active: activeWallet } = wallets;
    hardwareWallets.initiateTransaction({ walletId: activeWallet.id });
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
      hwDeviceStatus,
      isHardwareWallet,
    } = this.props;
    const { stores } = this.props;
    const { sendMoneyRequest, active: activeWallet } = stores.wallets;
    const {
      _resetTransaction: resetHardwareWalletTransaction,
      sendMoneyRequest: sendMoneyExternalRequest,
      isTransactionPending,
    } = stores.hardwareWallets;
    const { isFlight } = global;

    if (!activeWallet)
      throw new Error('Active wallet required for WalletSendPage.');

    const isSubmitting =
      (!isHardwareWallet && sendMoneyRequest.isExecuting) ||
      (isHardwareWallet &&
        (sendMoneyExternalRequest.isExecuting || isTransactionPending));
    const error =
      (!isHardwareWallet && sendMoneyRequest.error) ||
      (isHardwareWallet && sendMoneyExternalRequest.error);

    return (
      <WalletSendConfirmationDialog
        amount={amount}
        receiver={receiver}
        totalAmount={totalAmount}
        transactionFee={transactionFee}
        amountToNaturalUnits={amountToNaturalUnits}
        onSubmit={this.handleWalletSendFormSubmit}
        isSubmitting={isSubmitting}
        isFlight={isFlight}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          sendMoneyRequest.reset();
          resetHardwareWalletTransaction({ cancelDeviceAction: true });
        }}
        error={error}
        currencyUnit={currencyUnit}
        onExternalLinkClick={onExternalLinkClick}
        hwDeviceStatus={hwDeviceStatus}
        isHardwareWallet={isHardwareWallet}
        onInitiateTransaction={this.handleInitiateTransaction}
      />
    );
  }
}

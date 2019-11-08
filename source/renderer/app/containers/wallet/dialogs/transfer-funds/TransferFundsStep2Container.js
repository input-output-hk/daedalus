// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TransferFundsStep2Dialog from '../../../../components/wallet/transfer-funds/TransferFundsStep2Dialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class TransferFundsStep2Container extends Component<Props> {
  static defaultProps = DefaultProps;

  onClose = () => {
    const { onClose } = this.props;
    const { transferFundsRequest } = this.props.stores.wallets;

    if (transferFundsRequest.isExecuting) {
      return;
    }

    transferFundsRequest.reset();
    onClose();
  };

  render() {
    const { stores, actions, onBack } = this.props;
    const {
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
      allLegacyWallets,
      allWallets,
      transferFundsFee,
      transferFundsRequest,
    } = stores.wallets;

    const onFinish = spendingPassword =>
      actions.wallets.transferFunds.trigger({ spendingPassword });

    const sourceWallet = allLegacyWallets.find(
      ({ id }) => id === transferFundsSourceWalletId
    );
    const targetWallet = allWallets.find(
      ({ id }) => id === transferFundsTargetWalletId
    );
    if (!sourceWallet || !targetWallet) return null;
    return (
      <TransferFundsStep2Dialog
        addresses={[]}
        transferFundsFee={transferFundsFee}
        onBack={onBack}
        onClose={this.onClose}
        onFinish={onFinish}
        isSubmitting={transferFundsRequest.isExecuting}
        error={transferFundsRequest.error}
        sourceWallet={sourceWallet}
        targetWallet={targetWallet}
      />
    );
  }
}

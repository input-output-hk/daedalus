// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TransferFundsStep2Dialog from '../../../../components/wallet/transfer-funds/TransferFundsStep2Dialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores')
@observer
export default class TransferFundsStep1Container extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, onClose, onContinue, onBack } = this.props;
    const {
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
      allLegacyWallets,
      allWallets,
    } = stores.wallets;
    const walletFrom = allLegacyWallets.find(
      ({ id }) => id === transferFundsSourceWalletId
    );
    const walletTo = allWallets.find(
      ({ id }) => id === transferFundsTargetWalletId
    );
    if (!walletFrom || !walletTo) return null;
    return (
      <TransferFundsStep2Dialog
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
        addresses={[]}
        amount="3"
        fees="+ 12.042481"
        total="15.042481"
      />
    );
  }
}

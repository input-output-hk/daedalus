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
export default class TransferFundsStep1Container extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, actions, onClose, onContinue, onBack } = this.props;
    const {
      transferFundsSourceWalletId,
      transferFundsTargetWalletId,
      allLegacyWallets,
      allWallets,
    } = stores.wallets;

    const { updateDataForActiveDialog } = actions.dialogs;
    const { dataForActiveDialog } = stores.uiDialogs;

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
        fees={12.042481}
        onBack={onBack}
        onClose={onClose}
        onContinue={onContinue}
        onDataChange={data => {
          updateDataForActiveDialog.trigger({ data });
        }}
        sourceWallet={sourceWallet}
        spendingPasswordValue={dataForActiveDialog.spendingPasswordValue}
        targetWallet={targetWallet}
      />
    );
  }
}

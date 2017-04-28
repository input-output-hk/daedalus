// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletRestoreDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { recoveryPhrase: string, walletName: string }) => {
    this.props.actions.wallets.restoreWallet.trigger(values);
    this.props.actions.dialogs.closeActiveDialog.trigger();
    this.props.actions.wallets.resetRestoreWallet.trigger();
  };

  onCancel = () => {
    this.props.onClose();
    this.props.actions.wallets.resetRestoreWallet.trigger();
  };

  mnemonicValidator = (mnemonic: string) => {
    this.props.stores.wallets.isValidMnemonic(mnemonic);
  };

  render() {
    const { wallets } = this.props.stores;
    const { restoreRequest } = wallets;

    return (
      <WalletRestoreDialog
        onSubmit={this.onSubmit}
        error={restoreRequest.error}
        onCancel={this.onCancel}
        mnemonicValidator={this.mnemonicValidator}
      />
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import environment from '../../../environment';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
export default class WalletRestoreDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  onSubmit = (values: { recoveryPhrase: string, walletName: string, walletPassword: ?string }) => {
    this.props.actions[environment.API].wallets.restoreWallet.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    this.props.stores[environment.API].wallets.restoreRequest.reset();
  };

  render() {
    const { wallets } = this.props.stores[environment.API];
    const { restoreRequest } = wallets;

    return (
      <WalletRestoreDialog
        mnemonicValidator={mnemonic => wallets.isValidMnemonic(mnemonic)}
        isSubmitting={restoreRequest.isExecuting}
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        error={restoreRequest.error}
      />
    );
  }
}

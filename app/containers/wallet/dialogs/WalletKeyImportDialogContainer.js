// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletKeyImportDialog from '../../../components/wallet/key-import/WalletKeyImportDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletKeyImportDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { filePath: string }) => {
    this.props.actions.wallets.importWalletFromKey.trigger(values);
    this.props.actions.dialogs.closeActiveDialog.trigger();
    this.props.actions.wallets.resetImportWalletFromKey.trigger();
  };

  render() {
    const { wallets } = this.props.stores;
    const { importFromKeyRequest } = wallets;

    return (
      <WalletKeyImportDialog
        isSubmitting={importFromKeyRequest.isExecuting}
        onSubmit={this.onSubmit}
        error={importFromKeyRequest.error}
        onClose={this.props.onClose}
      />
    );
  }
}

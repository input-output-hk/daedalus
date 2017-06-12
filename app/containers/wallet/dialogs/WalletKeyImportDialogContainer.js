// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletKeyImportDialog from '../../../components/wallet/key-import/WalletKeyImportDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletKeyImportDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { filePath: string }) => {
    this.props.actions.wallets.importWalletFromKey.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    this.props.stores.wallets.importFromKeyRequest.reset();
  };

  render() {
    const { wallets } = this.props.stores;
    const { importFromKeyRequest } = wallets;

    return (
      <WalletKeyImportDialog
        isSubmitting={importFromKeyRequest.isExecuting}
        onSubmit={this.onSubmit}
        onClose={this.onCancel}
        error={importFromKeyRequest.error}
      />
    );
  }
}

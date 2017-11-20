// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletFileImportDialog from '../../../components/wallet/file-import/WalletFileImportDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletFileImportDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { filePath: string, walletPassword: ?string, walletName: ?string }) => {
    this.props.actions.ada.wallets.importWalletFromFile.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    // Import request should be reset only in case restore is finished/errored
    const { importFromFileRequest } = this.props.stores.ada.wallets;
    if (!importFromFileRequest.isExecuting) importFromFileRequest.reset();
  };

  render() {
    const { wallets } = this.props.stores.ada;
    const { importFromFileRequest } = wallets;

    return (
      <WalletFileImportDialog
        isSubmitting={importFromFileRequest.isExecuting}
        onSubmit={this.onSubmit}
        onClose={this.onCancel}
        error={importFromFileRequest.error}
      />
    );
  }
}

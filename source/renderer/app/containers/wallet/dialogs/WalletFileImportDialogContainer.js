// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletFileImportDialog from '../../../components/wallet/file-import/WalletFileImportDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletFileImportDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onSubmit = (values: {
    filePath: string,
    spendingPassword: string,
    walletName: ?string,
  }) => {
    this.props.actions.wallets.importWalletFromFile.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    // Import request should be reset only in case restore is finished/errored
    const { importFromFileRequest } = this.props.stores.wallets;
    if (!importFromFileRequest.isExecuting) importFromFileRequest.reset();
  };

  render() {
    const { wallets } = this.props.stores;
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

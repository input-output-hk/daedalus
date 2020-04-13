// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletImportFileDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onConfirm = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletSelectImportDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
    const { importFromFileRequest } = this.props.stores.wallets;
    if (!importFromFileRequest.isExecuting) importFromFileRequest.reset();
  };

  render() {
    const { app } = this.props.stores;
    const { openExternalLink } = app;

    return (
      <WalletImportFileDialog
        onConfirm={this.onConfirm}
        onClose={this.onCancel}
        openExternalLink={openExternalLink}
      />
    );
  }
}

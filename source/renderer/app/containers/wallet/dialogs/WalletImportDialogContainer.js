// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';
import type { StoresMap } from '../../../stores';
import type { ActionsMap } from '../../../actions';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
  isWalletFileImportDialog: boolean,
  isWalletSelectImportDialog: boolean,
};

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<Props> {
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
    const { isWalletFileImportDialog, isWalletSelectImportDialog } = this.props;
    const { app, networkStatus } = this.props.stores;
    const { stateDirectoryPath } = networkStatus;
    const { openExternalLink } = app;

    const onSelectStateDirectory = () => {};

    return (
      <>
        {isWalletFileImportDialog && (
          <WalletImportFileDialog
            onConfirm={this.onConfirm}
            onClose={this.onCancel}
            stateDirectoryPath={stateDirectoryPath}
            onOpenExternalLink={openExternalLink}
            onSelectStateDirectory={onSelectStateDirectory}
          />
        )}
        {isWalletSelectImportDialog && (
          <WalletSelectImportDialog
            onConfirm={this.onConfirm}
            onClose={this.onCancel}
          />
        )}
      </>
    );
  }
}

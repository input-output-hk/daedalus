// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
// import { remote } from 'electron';
import ExportWalletToFileDialog from '../../../../components/wallet/settings/ExportWalletToFileDialog';
import type { OnSubmitParams } from '../../../../components/wallet/settings/ExportWalletToFileDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class ExportWalletToFileDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onSubmit = (params: OnSubmitParams) => {
    // TODO: refactor this direct access to the dialog api
    const filePath = global.dialog.showSaveDialog({
      defaultPath: 'wallet-export.json',
      filters: [
        {
          name: 'wallet-export',
          extensions: ['json'],
        },
      ],
    });
    const { stores, actions } = this.props;
    const activeWallet = stores.wallets.active;
    if (!filePath || !activeWallet) return;
    actions.walletSettings.exportToFile.trigger({
      walletId: activeWallet.id,
      filePath,
      ...params,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
    this.props.stores.walletSettings.exportWalletToFileRequest.reset();
  };

  render() {
    const { wallets, walletSettings } = this.props.stores;
    const activeWallet = wallets.active;
    const { exportWalletToFileRequest } = walletSettings;

    // We need an active wallet
    if (!activeWallet) return null;

    return (
      <ExportWalletToFileDialog
        walletName={activeWallet.name}
        isSubmitting={exportWalletToFileRequest.isExecuting}
        onSubmit={this.onSubmit}
        onClose={this.onCancel}
        error={exportWalletToFileRequest.error}
      />
    );
  }
}

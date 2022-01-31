import React, { Component } from 'react';
import path from 'path';
import { observer, inject } from 'mobx-react';
import { showSaveDialogChannel } from '../../../../ipc/show-file-dialog-channels';
import ExportWalletToFileDialog from '../../../../components/wallet/settings/ExportWalletToFileDialog';
import type { OnSubmitParams } from '../../../../components/wallet/settings/ExportWalletToFileDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class ExportWalletToFileDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onSubmit = async (params: OnSubmitParams) => {
    const name = 'wallet-export';
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath = path.join(desktopDirectoryPath, `${name}.json`);
    const fileParams = {
      defaultPath,
      filters: [
        {
          name,
          extensions: ['json'],
        },
      ],
    };
    const { filePath } = await showSaveDialogChannel.send(fileParams);
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

export default ExportWalletToFileDialogContainer;

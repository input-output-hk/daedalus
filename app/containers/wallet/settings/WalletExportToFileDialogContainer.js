// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { remote } from 'electron';
import WalletExportDialog from '../../../components/wallet/settings/export-to-file/WalletExportToFileDialog';
import type { OnSubmitParams } from '../../../components/wallet/settings/export-to-file/WalletExportToFileDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletExportToFileDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = (params: OnSubmitParams) => {
    const filePath = remote.dialog.showSaveDialog({
      filters: [
        { name: 'Json', extensions: ['json'] },
      ]
    });
    const { stores, actions } = this.props;
    const activeWallet = stores.wallets.active;
    if (!filePath || !activeWallet) return;
    actions.walletSettings.exportToFile.trigger({
      walletId: activeWallet.id,
      filePath,
      ...params
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores } = this.props;
    const activeWallet = stores.wallets.active;

    // We need an active wallet
    if (!activeWallet) return null;

    return (
      <WalletExportDialog
        walletName={activeWallet.name}
        hasSpendingPassword={activeWallet.hasPassword}
        isSubmitting={false}
        onSubmit={this.onSubmit}
        onClose={this.onCancel}
      />
    );
  }

}

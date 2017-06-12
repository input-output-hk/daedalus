// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletExportDialog from '../../../components/wallet/settings/WalletExportDialog';
import ExportPaperWalletPrinterCopyDialog from '../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletExportDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onPrint = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletPrinterCopyDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores, actions } = this.props;

    return (
      <WalletExportDialog
        onPrint={this.onPrint}
        onClose={this.onCancel}
        onChooseWalletExportType={(choice) => {
          actions.wallets.chooseWalletExportType.trigger({ walletExportType: choice });
        }}
        walletExportType={stores.wallets.walletExportType}
      />
    );
  }

}

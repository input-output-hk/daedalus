// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletPrinterCopyDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
import ExportPaperWalletMnemonicDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import WalletExportDialog from '../../../../components/wallet/settings/export-to-file/WalletExportToFileDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
// eslint-disable-next-line max-len
export default class ExportPaperWalletPrinterCopyDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  onContinue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletMnemonicDialog,
    });
  };

  onBack = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletExportDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  onTogglePrinterCopyNotice = () => {
    const { uiDialogs } = this.props.stores;
    this.props.actions.dialogs.updateDataForActiveDialog.trigger({
      data: {
        isPrinterCopyNoticeAccepted: !uiDialogs.dataForActiveDialog.isPrinterCopyNoticeAccepted
      }
    });
  }

  render() {
    const { uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;

    return (
      <ExportPaperWalletPrinterCopyDialog
        onContinue={this.onContinue}
        onClose={this.onCancel}
        onBack={this.onBack}
        isPrinterCopyNoticeAccepted={dialogData.isPrinterCopyNoticeAccepted}
        onTogglePrinterCopyNotice={this.onTogglePrinterCopyNotice}
      />
    );
  }
}

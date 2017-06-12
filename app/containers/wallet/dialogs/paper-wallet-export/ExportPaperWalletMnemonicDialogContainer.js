// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletMnemonicDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import ExportPaperWalletMnemonicVerificationDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import ExportPaperWalletPrinterCopyDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class ExportPaperWalletMnemonicDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onContinue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletMnemonicVerificationDialog,
    });
  };

  onBack = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletPrinterCopyDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  onTogglePhraseWrittenNotice = () => {
    const { uiDialogs } = this.props.stores;
    this.props.actions.dialogs.updateDataForActiveDialog.trigger({
      data: {
        isPhraseWrittenNoticeAccepted: !uiDialogs.dataForActiveDialog.isPhraseWrittenNoticeAccepted
      }
    });
  }


  render() {
    const { stores } = this.props;
    const { wallets, uiDialogs } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;

    return (
      <ExportPaperWalletMnemonicDialog
        onContinue={this.onContinue}
        onBack={this.onBack}
        onClose={this.onCancel}
        isPhraseWrittenNoticeAccepted={dialogData.isPhraseWrittenNoticeAccepted}
        onTogglePhraseWrittenNotice={this.onTogglePhraseWrittenNotice}
        recoveryPhrase={wallets.walletExportMnemonic}
      />
    );
  }
}

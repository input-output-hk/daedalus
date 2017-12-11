// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletMnemonicDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import ExportPaperWalletMnemonicVerificationDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import ExportPaperWalletPrinterCopyDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletPrinterCopyDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
// eslint-disable-next-line max-len
export default class ExportPaperWalletMnemonicDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

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
    const { uiDialogs } = stores;
    const { wallets } = stores.ada;
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

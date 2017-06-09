// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletMnemonicVerificationDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import ExportPaperWalletCertificateDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletCertificateDialog';
import ExportPaperWalletMnemonicDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class ExportPaperWalletMnemonicVerificationDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null };

  props: InjectedDialogContainerProps;

  onContinue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletCertificateDialog,
    });
  };

  onBack = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletMnemonicDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { stores } = this.props;
    const { walletExportMnemonic } = stores.wallets;

    return (
      <ExportPaperWalletMnemonicVerificationDialog
        isSubmitting={false}
        onContinue={this.onContinue}
        onBack={this.onBack}
        onClose={this.onCancel}
        error={null}
        walletExportMnemonic={walletExportMnemonic}
      />
    );
  }

}

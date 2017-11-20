// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletMnemonicVerificationDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import ExportPaperWalletCertificateDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletCertificateDialog';
import ExportPaperWalletMnemonicDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
// eslint-disable-next-line max-len
export default class ExportPaperWalletMnemonicVerificationDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

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
    const { walletExportMnemonic } = stores.ada.wallets;

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

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import ExportPaperWalletCertificateDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletCertificateDialog';
import ExportPaperWalletMnemonicVerificationDialog from '../../../../components/wallet/settings/paper-wallet-export-dialogs/ExportPaperWalletMnemonicVerificationDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class ExportPaperWalletCertificateDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  onFinish = () => {
    this.onCancel();
  };

  onBack = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: ExportPaperWalletMnemonicVerificationDialog,
    });
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    return (
      <ExportPaperWalletCertificateDialog
        onFinish={this.onFinish}
        onBack={this.onBack}
        onClose={this.onCancel}
      />
    );
  }
}

import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import validWords from '../../../../../common/config/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../common/config/crypto/decrypt';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class WalletRestoreDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onSubmit = (values: {
    recoveryPhrase: string;
    walletName: string;
    spendingPassword: string;
    type?: string;
  }) => {
    this.props.actions.wallets.restoreWallet.trigger(values);
  };
  onCancel = () => {
    this.props.onClose();
    this.resetRequests();
  };
  resetRequests = () => {
    // Restore request should be reset only in case restore is finished/errored
    const { wallets } = this.props.stores;
    const {
      restoreDaedalusRequest,
      restoreLegacyRequest,
      getWalletRecoveryPhraseFromCertificateRequest,
    } = wallets;

    if (!restoreDaedalusRequest.isExecuting) {
      restoreDaedalusRequest.reset();
      restoreLegacyRequest.reset();
      getWalletRecoveryPhraseFromCertificateRequest.reset();
    }
  };

  render() {
    const { wallets } = this.props.stores;
    const {
      restoreDaedalusRequest,
      restoreLegacyRequest,
      getWalletRecoveryPhraseFromCertificateRequest,
    } = wallets;
    const error =
      restoreDaedalusRequest.error ||
      restoreLegacyRequest.error ||
      getWalletRecoveryPhraseFromCertificateRequest.error;
    const isExecuting =
      restoreDaedalusRequest.isExecuting ||
      restoreLegacyRequest.isExecuting ||
      getWalletRecoveryPhraseFromCertificateRequest.isExecuting;
    return (
      <WalletRestoreDialog
        mnemonicValidator={isValidMnemonic}
        suggestedMnemonics={validWords}
        isSubmitting={isExecuting}
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        onChoiceChange={this.resetRequests}
        error={error}
      />
    );
  }
}

export default WalletRestoreDialogContainer;

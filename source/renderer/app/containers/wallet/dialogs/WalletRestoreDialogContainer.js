// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import validWords from '../../../../../common/crypto/valid-words.en';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletRestoreDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onSubmit = (values: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: string,
    type?: string,
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
    const { restoreRequest } = wallets;
    if (!restoreRequest.isExecuting) {
      restoreRequest.reset();
      wallets.getWalletRecoveryPhraseFromCertificateRequest.reset();
    }
  };

  render() {
    const { wallets } = this.props.stores;
    const {
      restoreRequest,
      isValidMnemonic,
      getWalletRecoveryPhraseFromCertificateRequest,
    } = wallets;

    const error =
      restoreRequest.error ||
      getWalletRecoveryPhraseFromCertificateRequest.error;

    return (
      <WalletRestoreDialog
        mnemonicValidator={mnemonic => isValidMnemonic(mnemonic)}
        suggestedMnemonics={validWords}
        isSubmitting={restoreRequest.isExecuting}
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        onChoiceChange={this.resetRequests}
        error={error}
      />
    );
  }
}

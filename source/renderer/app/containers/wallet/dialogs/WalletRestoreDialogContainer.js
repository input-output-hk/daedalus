// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import environment from '../../../../../common/environment';
import validWords from '../../../../../common/valid-words.en';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
export default class WalletRestoreDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  onSubmit = (values: {
    recoveryPhrase: string,
    walletName: string,
    spendingPassword: ?string,
    type?: string,
  }) => {
    this.props.actions[environment.API].wallets.restoreWallet.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    this.resetRequests();
  };

  resetRequests = () => {
    // Restore request should be reset only in case restore is finished/errored
    const wallets = this._getWalletsStore();
    const { restoreRequest } = wallets;
    if (!restoreRequest.isExecuting) {
      restoreRequest.reset();
      if (environment.isAdaApi()) {
        wallets.getWalletRecoveryPhraseFromCertificateRequest.reset();
      }
    }
  };

  render() {
    const wallets = this._getWalletsStore();
    const { restoreRequest, isValidMnemonic } = wallets;

    const error = environment.isAdaApi()
      ? (restoreRequest.error || wallets.getWalletRecoveryPhraseFromCertificateRequest.error)
      : restoreRequest.error;

    return (
      <WalletRestoreDialog
        mnemonicValidator={mnemonic => isValidMnemonic(mnemonic)}
        showCertificateRestore={environment.isAdaApi()}
        suggestedMnemonics={validWords}
        isSubmitting={restoreRequest.isExecuting}
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        onChoiceChange={environment.isAdaApi() ? this.resetRequests : null}
        error={error}
      />
    );
  }

  _getWalletsStore() {
    return this.props.stores[environment.API].wallets;
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import SUGGESTED_MNEMONICS from '../../../../lib/valid-words.en.js';

@inject('stores', 'actions') @observer
export default class WalletRestoreDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { recoveryPhrase: string, walletName: string, walletPassword: ?string }) => {
    this.props.actions.wallets.restoreWallet.trigger(values);
  };

  onCancel = () => {
    this.props.onClose();
    this.props.stores.wallets.restoreRequest.reset();
  };

  render() {
    const { wallets } = this.props.stores;
    const { restoreRequest } = wallets;

    return (
      <WalletRestoreDialog
        mnemonicValidator={mnemonic => this.props.stores.wallets.isValidMnemonic(mnemonic)}
        suggestedMnemonics={SUGGESTED_MNEMONICS}
        isSubmitting={restoreRequest.isExecuting}
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        error={restoreRequest.error}
      />
    );
  }
}

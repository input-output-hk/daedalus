// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletKeyImportDialog from '../../components/wallet/key-import/WalletKeyImportDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletKeyImportPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  onSubmit = (values: { filePath: string }) => {
    this.props.actions.wallets.importWalletFromKey.trigger(values);
  };

  render() {
    const { wallets } = this.props.stores;
    const { importFromKeyRequest } = wallets;
    const { toggleWalletKeyImportDialog } = this.props.actions.wallets;

    return (
      <WalletKeyImportDialog
        isSubmitting={importFromKeyRequest.isExecuting}
        onSubmit={this.onSubmit}
        error={importFromKeyRequest.error}
        onClose={toggleWalletKeyImportDialog.trigger}
      />
    );
  }
}

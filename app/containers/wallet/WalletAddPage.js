// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletAddPage extends Component {

  props: InjectedProps;

  render() {
    const {
      toggleCreateWalletDialog,
      toggleWalletRestore,
      toggleAddWallet,
      toggleWalletKeyImportDialog
    } = this.props.actions.wallets;
    const { hasAnyWallets } = this.props.stores.wallets;
    return (
      <WalletAddDialog
        onCreate={toggleCreateWalletDialog.trigger}
        onRestore={toggleWalletRestore.trigger}
        onImportKey={toggleWalletKeyImportDialog.trigger}
        onCancel={toggleAddWallet.trigger}
        canClose={hasAnyWallets}
      />
    );
  }

}

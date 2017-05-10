// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../../components/wallet/WalletRestoreDialog';
import WalletKeyImportDialog from '../../components/wallet/key-import/WalletKeyImportDialog';
import WalletBackupDialog from '../../components/wallet/WalletBackupDialog';
import WalletKeyImportDialogContainer from '../wallet/dialogs/WalletKeyImportDialogContainer';
import WalletRestoreDialogContainer from '../wallet/dialogs/WalletRestoreDialogContainer';
import WalletBackupDialogContainer from '../wallet/dialogs/WalletBackupDialogContainer';
import WalletCreateDialogContainer from '../wallet/dialogs/WalletCreateDialogContainer';
import WalletAddDialogContainer from '../wallet/dialogs/WalletAddDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('actions', 'stores') @observer
export default class WalletAddPage extends Component {

  static defaultProps = { actions: null, stores: null };

  props: InjectedProps;

  onClose = () => {
    if (this.props.stores.wallets.hasAnyWallets) {
      this.props.actions.dialogs.closeActiveDialog.trigger();
    } else {
      this.props.actions.dialogs.open.trigger({
        dialog: WalletAddDialog,
      });
    }
  }

  render() {
    const { uiDialogs } = this.props.stores;
    let activeDialog = null;

    if (uiDialogs.isOpen(WalletAddDialog)) {
      activeDialog = (
        <WalletAddDialogContainer />
      );
    }

    if (uiDialogs.isOpen(WalletCreateDialog)) {
      activeDialog = (
        <WalletCreateDialogContainer
          onClose={this.onClose}
        />
      );
    }

    if (uiDialogs.isOpen(WalletBackupDialog)) {
      activeDialog = (
        <WalletBackupDialogContainer
          onClose={this.onClose}
        />
      );
    }

    if (uiDialogs.isOpen(WalletRestoreDialog)) {
      activeDialog = (
        <WalletRestoreDialogContainer
          onClose={this.onClose}
        />
      );
    }

    if (uiDialogs.isOpen(WalletKeyImportDialog)) {
      activeDialog = (
        <WalletKeyImportDialogContainer
          onClose={this.onClose}
        />
      );
    }

    return activeDialog;
  }

}

// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAdd from '../../components/wallet/WalletAdd';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../../components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../../components/wallet/file-import/WalletFileImportDialog';
import WalletBackupDialog from '../../components/wallet/WalletBackupDialog';
import WalletFileImportDialogContainer from '../wallet/dialogs/WalletFileImportDialogContainer';
import WalletRestoreDialogContainer from '../wallet/dialogs/WalletRestoreDialogContainer';
import WalletBackupDialogContainer from '../wallet/dialogs/WalletBackupDialogContainer';
import WalletCreateDialogContainer from '../wallet/dialogs/WalletCreateDialogContainer';
import Layout from '../../containers/MainLayout';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores') @observer
export default class WalletAddPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const wallets = this._getWalletsStore();
    const { actions, stores } = this.props;
    const { uiDialogs } = stores;
    const { isRestoreActive } = wallets;
    let content = null;

    if (uiDialogs.isOpen(WalletCreateDialog)) {
      content = <WalletCreateDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletBackupDialog)) {
      content = <WalletBackupDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletRestoreDialog)) {
      content = <WalletRestoreDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletFileImportDialog)) {
      content = <WalletFileImportDialogContainer onClose={this.onClose} />;
    } else {
      content = (
        <WalletAdd
          onCreate={() => actions.dialogs.open.trigger({ dialog: WalletCreateDialog })}
          onRestore={() => actions.dialogs.open.trigger({ dialog: WalletRestoreDialog })}
          onImportFile={() => actions.dialogs.open.trigger({ dialog: WalletFileImportDialog })}
          isRestoreActive={isRestoreActive}
          isMaxNumberOfWalletsReached={wallets.hasMaxWallets}
        />
      );
    }
    return <Layout>{content}</Layout>;
  }

  _getWalletsStore() {
    return this.props.stores.ada.wallets;
  }

}

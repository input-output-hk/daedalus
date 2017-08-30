// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletAddDialog from '../../../components/wallet/WalletAddDialog';
import WalletCreateDialog from '../../../components/wallet/WalletCreateDialog';
import WalletRestoreDialog from '../../../components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../../../components/wallet/file-import/WalletFileImportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletAddDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;
  render() {
    const { actions, stores } = this.props;
    return (
      <WalletAddDialog
        onCreate={() => actions.dialogs.open.trigger({
          dialog: WalletCreateDialog,
        })}
        onRestore={() => actions.dialogs.open.trigger({
          dialog: WalletRestoreDialog,
        })}
        onImportFile={() => actions.dialogs.open.trigger({
          dialog: WalletFileImportDialog,
        })}
        onCancel={actions.dialogs.closeActiveDialog.trigger}
        canClose={stores.wallets.hasAnyWallets}
      />
    );
  }

}

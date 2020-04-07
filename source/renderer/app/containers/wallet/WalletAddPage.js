// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAdd from '../../components/wallet/WalletAdd';
import WalletFileImportDialog from '../../components/wallet/file-import/WalletFileImportDialog';
import WalletBackupDialog from '../../components/wallet/WalletBackupDialog';
import WalletFileImportDialogContainer from './dialogs/WalletFileImportDialogContainer';
import WalletBackupDialogContainer from './dialogs/WalletBackupDialogContainer';
import WalletCreateDialogContainer from './dialogs/WalletCreateDialogContainer';
import WalletRestoreDialogContainer from './dialogs/WalletRestoreDialogContainer';
import Layout from '../MainLayout';
import type { InjectedProps } from '../../types/injectedPropsType';

// TODO: Remove once the new wallet creation process is ready
import WalletCreateDialogContainerOld from './dialogs/WalletCreateDialogContainerOld';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';

// TODO: Remove once the new wallet restoration process is ready
import WalletRestoreDialogContainerOld from './dialogs/WalletRestoreDialogContainerOld';
import WalletRestoreDialog from '../../components/wallet/WalletRestoreDialog';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class WalletAddPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets, uiDialogs } = stores;
    const {
      createWalletStep,
      createWalletUseNewProcess,
      restoreWalletStep,
      restoreWalletUseNewProcess,
      environment,
    } = wallets;
    const { isMainnet, isTestnet } = environment;

    const onCreateWallet = createWalletUseNewProcess
      ? () => actions.wallets.createWalletBegin.trigger()
      : // TODO: Remove once the new wallet creation process is ready
        () => actions.dialogs.open.trigger({ dialog: WalletCreateDialog });

    const onRestoreWallet = restoreWalletUseNewProcess
      ? () => actions.wallets.restoreWalletBegin.trigger()
      : // TODO: Remove once the new wallet restoration process is ready
        () => actions.dialogs.open.trigger({ dialog: WalletRestoreDialog });

    let content = null;

    // TODO: Remove once the new wallet creation process is ready
    if (uiDialogs.isOpen(WalletCreateDialog)) {
      content = <WalletCreateDialogContainerOld onClose={this.onClose} />;
      // ----
    } else if (createWalletStep !== null) {
      content = <WalletCreateDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletBackupDialog)) {
      content = <WalletBackupDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletRestoreDialog)) {
      content = <WalletRestoreDialogContainerOld onClose={this.onClose} />;
    } else if (restoreWalletStep !== null) {
      content = <WalletRestoreDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletFileImportDialog)) {
      content = <WalletFileImportDialogContainer onClose={this.onClose} />;
    } else {
      content = (
        <WalletAdd
          onCreate={onCreateWallet}
          onRestore={onRestoreWallet}
          onImportFile={() =>
            actions.dialogs.open.trigger({ dialog: WalletFileImportDialog })
          }
          isMaxNumberOfWalletsReached={wallets.hasMaxWallets}
          isMainnet={isMainnet}
          isTestnet={isTestnet}
        />
      );
    }
    return <Layout>{content}</Layout>;
  }
}

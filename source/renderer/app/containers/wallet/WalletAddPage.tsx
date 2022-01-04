import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAdd from '../../components/wallet/WalletAdd';
import WalletBackupDialog from '../../components/wallet/WalletBackupDialog';
import WalletBackupDialogContainer from './dialogs/WalletBackupDialogContainer';
import WalletCreateDialogContainer from './dialogs/WalletCreateDialogContainer';
import WalletRestoreDialogContainer from './dialogs/WalletRestoreDialogContainer';
import WalletConnectDialog from '../../components/wallet/WalletConnectDialog';
import WalletConnectDialogContainer from './dialogs/WalletConnectDialogContainer';
import Layout from '../MainLayout';
import type { InjectedProps } from '../../types/injectedPropsType';
// TODO: Remove once the new wallet creation process is ready
import WalletCreateDialogContainerOld from './dialogs/WalletCreateDialogContainerOld';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';
// TODO: Remove once the new wallet restoration process is ready
import WalletRestoreDialogContainerOld from './dialogs/WalletRestoreDialogContainerOld';
import WalletRestoreDialog from '../../components/wallet/WalletRestoreDialog';
import WalletImportDialogContainer from './dialogs/WalletImportDialogContainer';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
class WalletAddPage extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  onClose = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets, walletMigration, uiDialogs } = stores;
    const {
      createWalletStep,
      createWalletUseNewProcess,
      restoreWalletStep,
      restoreWalletUseNewProcess,
      environment,
    } = wallets;
    const { walletMigrationStep } = walletMigration;
    const { isMainnet, isTestnet, isProduction } = environment;
    const onCreateWallet = createWalletUseNewProcess
      ? () => actions.wallets.createWalletBegin.trigger() // TODO: Remove once the new wallet creation process is ready
      : () =>
          actions.dialogs.open.trigger({
            dialog: WalletCreateDialog,
          });
    const onRestoreWallet = restoreWalletUseNewProcess
      ? () => actions.wallets.restoreWalletBegin.trigger() // TODO: Remove once the new wallet restoration process is ready
      : () =>
          actions.dialogs.open.trigger({
            dialog: WalletRestoreDialog,
          });

    const onImportWallet = () =>
      actions.walletMigration.initiateMigration.trigger();

    const onConnectWallet = () => {
      actions.dialogs.open.trigger({
        dialog: WalletConnectDialog,
      });
      stores.hardwareWallets.establishHardwareWalletConnection();
    };

    let activeDialog = null;

    // TODO: Remove once the new wallet creation process is ready
    if (uiDialogs.isOpen(WalletCreateDialog)) {
      activeDialog = <WalletCreateDialogContainerOld onClose={this.onClose} />;
    } else if (createWalletStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = <WalletCreateDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletBackupDialog)) {
      activeDialog = <WalletBackupDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletRestoreDialog)) {
      activeDialog = <WalletRestoreDialogContainerOld onClose={this.onClose} />;
    } else if (restoreWalletStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = <WalletRestoreDialogContainer onClose={this.onClose} />;
    } else if (walletMigrationStep !== null) {
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      activeDialog = <WalletImportDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletConnectDialog)) {
      activeDialog = <WalletConnectDialogContainer onClose={this.onClose} />;
    }

    return (
      <Layout>
        <WalletAdd
          onCreate={onCreateWallet}
          onRestore={onRestoreWallet}
          onImport={onImportWallet}
          onConnect={onConnectWallet}
          isMaxNumberOfWalletsReached={wallets.hasMaxWallets}
          isMainnet={isMainnet}
          isTestnet={isTestnet}
          isProduction={isProduction}
        />
        {activeDialog}
      </Layout>
    );
  }
}

export default WalletAddPage;

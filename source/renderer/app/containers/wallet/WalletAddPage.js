// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletAdd from '../../components/wallet/WalletAdd';
import WalletRestoreDialog from '../../components/wallet/WalletRestoreDialog';
import WalletFileImportDialog from '../../components/wallet/file-import/WalletFileImportDialog';
import WalletBackupDialog from '../../components/wallet/WalletBackupDialog';
import WalletFileImportDialogContainer from './dialogs/WalletFileImportDialogContainer';
import WalletRestoreDialogContainer from './dialogs/WalletRestoreDialogContainer';
import WalletBackupDialogContainer from './dialogs/WalletBackupDialogContainer';
import WalletCreateDialogContainer from './dialogs/WalletCreateDialogContainer';
import Layout from '../MainLayout';
import type { InjectedProps } from '../../types/injectedPropsType';

// TODO: Remove once the new wallet creation process is ready
import WalletCreateDialogContainerOld from './dialogs/WalletCreateDialogContainerOld';
import WalletCreateDialog from '../../components/wallet/WalletCreateDialog';

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
    const { wallets, uiDialogs, app, networkStatus } = stores;
    const { createWalletStep, useNewWalletCreationProcess } = wallets;
    const { isIncentivizedTestnet } = networkStatus;
    const {
      environment: { isMainnet, isTestnet },
    } = app;

    const onWalletAdd = useNewWalletCreationProcess
      ? () => actions.wallets.createWalletBegin.trigger()
      : // TODO: Remove once the new wallet creation process is ready
        () => actions.dialogs.open.trigger({ dialog: WalletCreateDialog });

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
      content = <WalletRestoreDialogContainer onClose={this.onClose} />;
    } else if (uiDialogs.isOpen(WalletFileImportDialog)) {
      content = <WalletFileImportDialogContainer onClose={this.onClose} />;
    } else {
      content = (
        <WalletAdd
          isMainnet={isMainnet}
          isTestnet={isTestnet}
          onCreate={onWalletAdd}
          onRestore={() =>
            actions.dialogs.open.trigger({ dialog: WalletRestoreDialog })
          }
          onImportFile={() =>
            actions.dialogs.open.trigger({ dialog: WalletFileImportDialog })
          }
          isMaxNumberOfWalletsReached={wallets.hasMaxWallets}
          isIncentivizedTestnet={isIncentivizedTestnet}
        />
      );
    }
    return <Layout>{content}</Layout>;
  }
}

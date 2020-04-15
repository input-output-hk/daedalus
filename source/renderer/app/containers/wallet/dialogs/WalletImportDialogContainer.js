// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onConfirm = () => {
    this.props.actions.walletMigration.nextStep.trigger();
  };

  onCancel = () => {
    this.props.actions.walletMigration.finishMigration.trigger();
  };

  onWalletNameChange = (params: { id: string, name: string }) => {
    this.props.actions.walletMigration.updateWalletName.trigger(params);
  };

  onToggleWalletImportSelection = (id: string) => {
    this.props.actions.walletMigration.toggleWalletImportSelection.trigger(id);
  };

  render() {
    const { app, networkStatus, walletMigration } = this.props.stores;
    const {
      isExportRunning,
      exportedWallets,
      exportErrors,
      isRestorationRunning,
      walletMigrationStep,
    } = walletMigration;
    const { stateDirectoryPath } = networkStatus;
    const { openExternalLink } = app;

    const onSelectStateDirectory = () => {};

    return (
      <>
        {walletMigrationStep === 1 && (
          <WalletImportFileDialog
            isSubmitting={isExportRunning}
            exportErrors={exportErrors}
            onConfirm={this.onConfirm}
            onClose={this.onCancel}
            stateDirectoryPath={stateDirectoryPath}
            onOpenExternalLink={openExternalLink}
            onSelectStateDirectory={onSelectStateDirectory}
          />
        )}
        {walletMigrationStep === 2 && (
          <WalletSelectImportDialog
            exportedWallets={exportedWallets}
            isSubmitting={isRestorationRunning}
            onConfirm={this.onConfirm}
            onWalletNameChange={this.onWalletNameChange}
            onToggleWalletImportSelection={this.onToggleWalletImportSelection}
            onClose={this.onCancel}
          />
        )}
      </>
    );
  }
}

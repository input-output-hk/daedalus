// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';
import { isValidWalletName } from '../../../utils/validations';
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

  onSelectExportSourcePath = () => {
    this.props.actions.walletMigration.selectExportSourcePath.trigger();
  };

  render() {
    const { app, walletMigration } = this.props.stores;
    const {
      exportedWallets,
      exportErrors,
      exportSourcePath,
      pendingImportWalletsCount,
      isExportRunning,
      isRestorationRunning,
      walletMigrationStep,
    } = walletMigration;
    const { openExternalLink } = app;

    return (
      <>
        {walletMigrationStep === 1 && (
          <WalletImportFileDialog
            isSubmitting={isExportRunning}
            exportSourcePath={exportSourcePath}
            exportErrors={exportErrors}
            pendingImportWalletsCount={pendingImportWalletsCount}
            onConfirm={this.onConfirm}
            onClose={this.onCancel}
            onOpenExternalLink={openExternalLink}
            onSelectExportSourcePath={this.onSelectExportSourcePath}
          />
        )}
        {walletMigrationStep === 2 && (
          <WalletSelectImportDialog
            isSubmitting={isRestorationRunning}
            nameValidator={name => isValidWalletName(name)}
            exportedWallets={exportedWallets}
            pendingImportWalletsCount={pendingImportWalletsCount}
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

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';
import { isValidWalletName } from '../../../utils/validations';
import type { InjectedProps } from '../../../types/injectedPropsType';
import type { ImportFromOption } from '../../../types/walletExportTypes';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  componentWillMount() {
    // Reset migration data
    this.props.actions.walletMigration.resetMigration.trigger();
  }

  onConfirm = () => {
    this.props.actions.walletMigration.nextStep.trigger();
  };

  onCancel = () => {
    this.props.actions.walletMigration.finishMigration.trigger();
  };

  onWalletNameChange = (params: { index: number, name: string }) => {
    this.props.actions.walletMigration.updateWalletName.trigger(params);
  };

  onToggleWalletImportSelection = (params: { index: number }) => {
    this.props.actions.walletMigration.toggleWalletImportSelection.trigger(
      params
    );
  };

  onSelectExportSourcePath = (params: { importFrom: ImportFromOption }) => {
    this.props.actions.walletMigration.selectExportSourcePath.trigger(params);
  };

  onResetExportSourcePath = () => {
    this.props.actions.walletMigration.resetExportSourcePath.trigger();
  };

  render() {
    const { app, walletMigration } = this.props.stores;
    const {
      exportedWallets,
      exportErrors,
      exportSourcePath,
      defaultExportSourcePath,
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
            defaultExportSourcePath={defaultExportSourcePath}
            exportErrors={exportErrors}
            pendingImportWalletsCount={pendingImportWalletsCount}
            onConfirm={this.onConfirm}
            onClose={this.onCancel}
            onOpenExternalLink={openExternalLink}
            onSelectExportSourcePath={this.onSelectExportSourcePath}
            onResetExportSourcePath={this.onResetExportSourcePath}
          />
        )}
        {walletMigrationStep === 2 && (
          <WalletSelectImportDialog
            isSubmitting={isRestorationRunning}
            nameValidator={name => isValidWalletName(name)}
            exportedWallets={exportedWallets}
            pendingImportWalletsCount={pendingImportWalletsCount}
            onConfirm={this.onConfirm}
            onOpenExternalLink={openExternalLink}
            onWalletNameChange={this.onWalletNameChange}
            onToggleWalletImportSelection={this.onToggleWalletImportSelection}
            onClose={this.onCancel}
          />
        )}
      </>
    );
  }
}

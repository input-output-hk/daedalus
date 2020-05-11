// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { map } from 'lodash';
import WalletImportFileDialog from '../../../components/wallet/wallet-import/WalletImportFileDialog';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';
import { isValidWalletName } from '../../../utils/validations';
import { IMPORT_WALLET_STEPS } from '../../../config/walletRestoreConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';
import type { ImportFromOption } from '../../../types/walletExportTypes';
import { MAX_ADA_WALLETS_COUNT } from '../../../config/numbersConfig';
import { WalletImportStatuses } from '../../../types/walletExportTypes';

type Props = InjectedProps;

type State = {
  existingWalletsCount: number,
};

@inject('stores', 'actions')
@observer
export default class WalletImportDialogContainer extends Component<
  Props,
  State
> {
  static defaultProps = { actions: null, stores: null };

  state = {
    existingWalletsCount: this.props.stores.wallets.all.length,
  };

  get containers() {
    return {
      walletImportFile: WalletImportFileDialog,
      walletSelectImport: WalletSelectImportDialog,
    };
  }

  componentWillUnmount() {
    this.props.actions.walletMigration.finishMigration.trigger();
  }

  onOpen = () => {
    this.props.actions.walletMigration.resetMigration.trigger();
  };

  onConfirm = () => {
    this.props.actions.walletMigration.nextStep.trigger();
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
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
    const { stores, actions } = this.props;
    const { app, walletMigration } = stores;

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
    let walletsCount =
      this.state.existingWalletsCount + pendingImportWalletsCount;
    map(exportedWallets, wallet => {
      if (
        wallet.import.status === WalletImportStatuses.COMPLETED ||
        wallet.import.status === WalletImportStatuses.RUNNING
      ) {
        walletsCount++;
      }
    });
    const isMaxNumberOfWalletsReached = walletsCount >= MAX_ADA_WALLETS_COUNT;

    const { importWalletStep } = stores.wallets;

    const { restoreWalletClose, restoreWalletChangeStep } = actions.wallets;

    const stepId = IMPORT_WALLET_STEPS[importWalletStep];

    const CurrentContainer = this.containers[stepId];

    return (
      <CurrentContainer
        onContinue={() => restoreWalletChangeStep.trigger()}
        onBack={() => restoreWalletChangeStep.trigger(true)}
        onClose={() => restoreWalletClose.trigger()}
      />
      /* {walletMigrationStep === 1 && (
          <WalletImportFileDialog
            isSubmitting={isExportRunning}
            exportSourcePath={exportSourcePath}
            defaultExportSourcePath={defaultExportSourcePath}
            exportErrors={exportErrors}
            pendingImportWalletsCount={pendingImportWalletsCount}
            onOpen={this.onOpen}
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
            isMaxNumberOfWalletsReached={isMaxNumberOfWalletsReached}
          />
        )} */
    );
  }
}

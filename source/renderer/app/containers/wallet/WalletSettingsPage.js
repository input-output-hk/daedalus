// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import DeleteWalletDialogContainer from './dialogs/settings/DeleteWalletDialogContainer';
import ExportWalletToFileDialogContainer from './dialogs/settings/ExportWalletToFileDialogContainer';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs, wallets, walletSettings, app } = this.props.stores;
    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const activeWallet = wallets.active;
    const {
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
    } = actions.walletSettings;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');

    return (
      <WalletSettings
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        isSpendingPasswordSet={activeWallet.hasPassword}
        spendingPasswordUpdateDate={activeWallet.passwordUpdateDate}
        isDialogOpen={uiDialogs.isOpen}
        walletName={activeWallet.name}
        isSubmitting={updateWalletRequest.isExecuting}
        isInvalid={
          updateWalletRequest.wasExecuted &&
          updateWalletRequest.result === false
        }
        showExportLink={!isProduction}
        lastUpdatedField={lastUpdatedWalletField}
        onFieldValueChange={(field, value) =>
          updateWalletField.trigger({ field, value })
        }
        onStartEditing={field => startEditingWalletField.trigger({ field })}
        onStopEditing={stopEditingWalletField.trigger}
        onCancelEditing={cancelEditingWalletField.trigger}
        activeField={walletFieldBeingEdited}
        nameValidator={name => isValidWalletName(name)}
        changeSpendingPasswordDialog={<ChangeSpendingPasswordDialogContainer />}
        deleteWalletDialogContainer={<DeleteWalletDialogContainer />}
        exportWalletDialogContainer={<ExportWalletToFileDialogContainer />}
      />
    );
  }
}

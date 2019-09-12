// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
// import { getWalletLocalData } from '../../utils/walletLocalStorage.js';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import DeleteWalletDialogContainer from './dialogs/settings/DeleteWalletDialogContainer';
import ExportWalletToFileDialogContainer from './dialogs/settings/ExportWalletToFileDialogContainer';
import WalletRecoveryPhraseStep1Container from './dialogs/settings/WalletRecoveryPhraseStep1Container';
import WalletRecoveryPhraseStep2Container from './dialogs/settings/WalletRecoveryPhraseStep2Container';
import WalletRecoveryPhraseStep3Container from './dialogs/settings/WalletRecoveryPhraseStep3Container';
import WalletRecoveryPhraseStep4Container from './dialogs/settings/WalletRecoveryPhraseStep4Container';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs, walletSettings, app, wallets } = this.props.stores;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');

    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const {
      WALLET_ASSURANCE_LEVEL_OPTIONS,
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

    return (
      <WalletSettings
        assuranceLevels={WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={activeWallet.assurance}
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        isSpendingPasswordSet={activeWallet.hasPassword}
        spendingPasswordUpdateDate={activeWallet.passwordUpdateDate}
        mnemonicsConfirmationDate={activeWallet.mnemonicsConfirmationDate}
        isDialogOpen={uiDialogs.isOpen}
        walletId={activeWallet.id}
        walletName={activeWallet.name}
        walletCreationDate={activeWallet.createdAt}
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
        walletRecoveryPhraseStep1Container={
          <WalletRecoveryPhraseStep1Container />
        }
        walletRecoveryPhraseStep2Container={
          <WalletRecoveryPhraseStep2Container />
        }
        walletRecoveryPhraseStep3Container={
          <WalletRecoveryPhraseStep3Container />
        }
        walletRecoveryPhraseStep4Container={
          <WalletRecoveryPhraseStep4Container />
        }
      />
    );
  }
}

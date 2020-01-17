// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
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
    const {
      uiDialogs,
      walletSettings,
      app,
      wallets,
      networkStatus,
      profile,
    } = this.props.stores;
    const activeWallet = wallets.active;
    let isLegacyWallet: boolean = false;
    if (activeWallet) {
      isLegacyWallet = activeWallet.isLegacy;
    }

    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');

    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const {
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
      forceWalletResyncRequest,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
      forceWalletResync,
    } = actions.walletSettings;
    const { getWalletRecoveryPhraseVerification } = wallets;
    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = getWalletRecoveryPhraseVerification(activeWallet.id);

    const locale = profile.currentLocale;

    return (
      <WalletSettings
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        spendingPasswordUpdateDate={activeWallet.passwordUpdateDate}
        recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
        recoveryPhraseVerificationStatus={recoveryPhraseVerificationStatus}
        recoveryPhraseVerificationStatusType={
          recoveryPhraseVerificationStatusType
        }
        isDialogOpen={uiDialogs.isOpen}
        isLegacy={isLegacyWallet}
        walletId={activeWallet.id}
        walletName={activeWallet.name}
        creationDate={creationDate}
        isIncentivizedTestnet={networkStatus.isIncentivizedTestnet}
        isSubmitting={updateWalletRequest.isExecuting}
        isResyncing={forceWalletResyncRequest.isExecuting}
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
        onResyncWallet={() =>
          forceWalletResync.trigger({ walletId: activeWallet.id })
        }
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
        locale={locale}
      />
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import WalletRecoveryPhraseContainer from './dialogs/settings/WalletRecoveryPhraseContainer';
import DeleteWalletDialogContainer from './dialogs/settings/DeleteWalletDialogContainer';
import ExportWalletToFileDialogContainer from './dialogs/settings/ExportWalletToFileDialogContainer';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../config/cryptoConfig';

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
      isForcedWalletResyncStarting,
      getWalletsRecoveryPhraseVerificationData,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
      forceWalletResync,
      recoveryPhraseVerificationContinue,
    } = actions.walletSettings;

    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = getWalletsRecoveryPhraseVerificationData(activeWallet.id);

    const wordCount =
      activeWallet.discovery === 'random'
        ? LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
        : WALLET_RECOVERY_PHRASE_WORD_COUNT;

    const locale = profile.currentLocale;
    const { isIncentivizedTestnet } = global;

    return (
      <>
        <WalletSettings
          error={updateWalletRequest.error}
          openDialogAction={actions.dialogs.open.trigger}
          isSpendingPasswordSet={activeWallet.hasPassword}
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
          isIncentivizedTestnet={isIncentivizedTestnet}
          isSubmitting={updateWalletRequest.isExecuting}
          isForcedWalletResyncStarting={isForcedWalletResyncStarting}
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
            forceWalletResync.trigger({
              walletId: activeWallet.id,
              isLegacy: activeWallet.isLegacy,
            })
          }
          onVerifyRecoveryPhrase={recoveryPhraseVerificationContinue.trigger}
          activeField={walletFieldBeingEdited}
          nameValidator={name => isValidWalletName(name)}
          changeSpendingPasswordDialog={
            <ChangeSpendingPasswordDialogContainer />
          }
          deleteWalletDialogContainer={<DeleteWalletDialogContainer />}
          exportWalletDialogContainer={<ExportWalletToFileDialogContainer />}
          locale={locale}
          wordCount={wordCount}
        />
        {!isIncentivizedTestnet && <WalletRecoveryPhraseContainer />}
      </>
    );
  }
}

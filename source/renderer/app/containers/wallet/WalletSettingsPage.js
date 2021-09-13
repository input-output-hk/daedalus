// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/settings/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';
import { ellipsis } from '../../utils/strings';
import ChangeSpendingPasswordDialogContainer from './dialogs/settings/ChangeSpendingPasswordDialogContainer';
import WalletRecoveryPhraseContainer from './dialogs/settings/WalletRecoveryPhraseContainer';
import PublicKeyDialogContainer from './dialogs/settings/PublicKeyDialogContainer';
import PublicKeyQRCodeDialogContainer from './dialogs/settings/PublicKeyQRCodeDialogContainer';
import UndelegateWalletDialogContainer from './dialogs/settings/UndelegateWalletDialogContainer';
import DeleteWalletDialogContainer from './dialogs/settings/DeleteWalletDialogContainer';
import ExportWalletToFileDialogContainer from './dialogs/settings/ExportWalletToFileDialogContainer';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../config/cryptoConfig';
import { ROUTES } from '../../routes-config';
import { WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH } from '../../config/walletsConfig';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleCopyWalletPublicKey = (walletPublicKey: string) => {
    const { wallets } = this.props.actions;
    const publicKey = ellipsis(
      walletPublicKey,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );
    wallets.copyWalletPublicKey.trigger({ publicKey });
  };

  handleCopyICOPublicKey = (icoPublicKey: string) => {
    const { wallets } = this.props.actions;
    const publicKey = ellipsis(
      icoPublicKey,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );
    wallets.copyICOPublicKey.trigger({ publicKey });
  };

  handleDelegateClick = () => {
    const { goToRoute } = this.props.actions.router;
    goToRoute.trigger({ route: ROUTES.STAKING.DELEGATION_CENTER });
  };

  render() {
    const {
      uiDialogs,
      walletSettings,
      app,
      wallets,
      profile,
      hardwareWallets,
    } = this.props.stores;
    const { checkIsTrezorByWalletId } = hardwareWallets;
    const {
      active: activeWallet,
      activePublicKey: activeWalletPublicKey,
      icoPublicKey,
    } = wallets;
    // Guard against potential null values
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSettingsPage.');

    const { isLegacy, isHardwareWallet } = activeWallet;
    const isTrezor = checkIsTrezorByWalletId(activeWallet.id);
    const { actions } = this.props;
    const {
      environment: { isProduction },
    } = app;
    const {
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
      getWalletsRecoveryPhraseVerificationData,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
      recoveryPhraseVerificationContinue,
    } = actions.walletSettings;
    const {
      creationDate,
      recoveryPhraseVerificationDate,
      recoveryPhraseVerificationStatus,
      recoveryPhraseVerificationStatusType,
    } = getWalletsRecoveryPhraseVerificationData(activeWallet.id);
    const locale = profile.currentLocale;
    const shouldDisplayRecoveryPhrase = !isHardwareWallet;
    const wordCount = activeWallet.isRandom
      ? LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
      : WALLET_RECOVERY_PHRASE_WORD_COUNT;

    return (
      <>
        <WalletSettings
          error={updateWalletRequest.error}
          openDialogAction={actions.dialogs.open.trigger}
          isHardwareWallet={isHardwareWallet}
          isSpendingPasswordSet={activeWallet.hasPassword}
          isDelegating={activeWallet.isDelegating}
          spendingPasswordUpdateDate={activeWallet.passwordUpdateDate}
          recoveryPhraseVerificationDate={recoveryPhraseVerificationDate}
          recoveryPhraseVerificationStatus={recoveryPhraseVerificationStatus}
          recoveryPhraseVerificationStatusType={
            recoveryPhraseVerificationStatusType
          }
          isDialogOpen={uiDialogs.isOpen}
          isLegacy={isLegacy}
          walletId={activeWallet.id}
          walletName={activeWallet.name}
          delegationStakePoolStatus={activeWallet.delegationStakePoolStatus}
          lastDelegationStakePoolStatus={
            activeWallet.lastDelegationStakePoolStatus
          }
          isRestoring={activeWallet.isRestoring}
          isSyncing={activeWallet.isSyncing}
          walletPublicKey={activeWalletPublicKey}
          icoPublicKey={icoPublicKey}
          creationDate={creationDate}
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
          onStartEditing={(field) => startEditingWalletField.trigger({ field })}
          onStopEditing={stopEditingWalletField.trigger}
          onCancel={cancelEditingWalletField.trigger}
          onVerifyRecoveryPhrase={recoveryPhraseVerificationContinue.trigger}
          onCopyWalletPublicKey={this.handleCopyWalletPublicKey}
          onCopyICOPublicKey={this.handleCopyICOPublicKey}
          updateDataForActiveDialogAction={
            actions.dialogs.updateDataForActiveDialog.trigger
          }
          onDelegateClick={this.handleDelegateClick}
          activeField={walletFieldBeingEdited}
          nameValidator={(name) => isValidWalletName(name)}
          changeSpendingPasswordDialog={
            <ChangeSpendingPasswordDialogContainer />
          }
          walletPublicKeyDialogContainer={<PublicKeyDialogContainer />}
          icoPublicKeyDialogContainer={<PublicKeyDialogContainer isICO />}
          walletPublicKeyQRCodeDialogContainer={
            <PublicKeyQRCodeDialogContainer />
          }
          icoPublicKeyQRCodeDialogContainer={
            <PublicKeyQRCodeDialogContainer isICO />
          }
          undelegateWalletDialogContainer={
            <UndelegateWalletDialogContainer
              onExternalLinkClick={app.openExternalLink}
              isTrezor={isHardwareWallet && isTrezor}
            />
          }
          deleteWalletDialogContainer={<DeleteWalletDialogContainer />}
          exportWalletDialogContainer={<ExportWalletToFileDialogContainer />}
          locale={locale}
          wordCount={wordCount}
          shouldDisplayRecoveryPhrase={shouldDisplayRecoveryPhrase}
        />
        {shouldDisplayRecoveryPhrase && <WalletRecoveryPhraseContainer />}
      </>
    );
  }
}

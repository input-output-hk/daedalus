// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletBackupDialog from '../../../components/wallet/WalletBackupDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import environment from '../../../environment';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
export default class WalletBackupDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  onCancelBackup = () => {
    this.props.onClose();
    this.props.actions.walletBackup.cancelWalletBackup.trigger();
  }

  render() {
    const { actions, stores } = this.props;
    const {
      recoveryPhraseWords,
      enteredPhrase,
      isRecoveryPhraseValid,
      countdownRemaining,
      recoveryPhraseShuffled,
      isTermDeviceAccepted,
      isTermRecoveryAccepted,
      isPrivacyNoticeAccepted,
      currentStep
    } = stores.walletBackup;
    const {
      startWalletBackup,
      addWordToWalletBackupVerification,
      clearEnteredRecoveryPhrase,
      acceptWalletBackupTermDevice,
      acceptWalletBackupTermRecovery,
      restartWalletBackup,
      finishWalletBackup,
      acceptPrivacyNoticeForWalletBackup,
      continueToRecoveryPhraseForWalletBackup
    } = actions.walletBackup;
    const { createWalletRequest } = stores[environment.API].wallets;
    return (
      <WalletBackupDialog
        // Global props for all dialogs
        currentStep={currentStep}
        onCancelBackup={this.onCancelBackup}
        // Props for WalletBackupPrivacyWarningDialog
        canPhraseBeShown={isPrivacyNoticeAccepted && countdownRemaining === 0}
        isPrivacyNoticeAccepted={isPrivacyNoticeAccepted}
        countdownRemaining={countdownRemaining}
        onAcceptPrivacyNotice={acceptPrivacyNoticeForWalletBackup.trigger}
        onContinue={continueToRecoveryPhraseForWalletBackup.trigger}
        // Props for WalletRecoveryPhraseDisplayDialog
        recoveryPhrase={recoveryPhraseWords.reduce((phrase, { word }) => `${phrase} ${word}`, '')}
        onStartWalletBackup={startWalletBackup.trigger}
        // Props for WalletRecoveryPhraseEntryDialog
        isTermDeviceAccepted={isTermDeviceAccepted}
        enteredPhrase={enteredPhrase}
        canFinishBackup={isRecoveryPhraseValid && isTermDeviceAccepted && isTermRecoveryAccepted}
        isTermRecoveryAccepted={isTermRecoveryAccepted}
        isValid={isRecoveryPhraseValid}
        isSubmitting={createWalletRequest.isExecuting}
        onAcceptTermDevice={acceptWalletBackupTermDevice.trigger}
        onAcceptTermRecovery={acceptWalletBackupTermRecovery.trigger}
        onAddWord={addWordToWalletBackupVerification.trigger}
        onClear={clearEnteredRecoveryPhrase.trigger}
        onFinishBackup={() => {
          finishWalletBackup.trigger();
        }}
        onRestartBackup={restartWalletBackup.trigger}
        recoveryPhraseShuffled={recoveryPhraseShuffled}
      />
    );
  }
}

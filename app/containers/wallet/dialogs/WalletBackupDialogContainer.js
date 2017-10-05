// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletBackupDialog from '../../../components/wallet/WalletBackupDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletBackupDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

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
    } = stores.ada.walletBackup;
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

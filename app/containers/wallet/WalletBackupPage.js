// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletBackupPrivacyWarningDialog from '../../components/wallet/backup-recovery/WalletBackupPrivacyWarningDialog';
import WalletRecoveryPhraseDisplayDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseDisplayDialog';
import WalletRecoveryPhraseEntryDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseEntryDialog';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletBackupPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
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
    } = this.props.stores.walletBackup;
    const {
      startWalletBackup,
      addWordToWalletBackupVerification,
      clearEnteredRecoveryPhrase,
      acceptWalletBackupTermDevice,
      acceptWalletBackupTermRecovery,
      restartWalletBackup,
      cancelWalletBackup,
      finishWalletBackup,
      acceptPrivacyNoticeForWalletBackup,
      continueToRecoveryPhraseForWalletBackup
    } = this.props.actions.walletBackup;

    if (currentStep === 'privacyWarning') {
      return (
        <WalletBackupPrivacyWarningDialog
          canPhraseBeShown={isPrivacyNoticeAccepted && countdownRemaining === 0}
          isPrivacyNoticeAccepted={isPrivacyNoticeAccepted}
          countdownRemaining={countdownRemaining}
          onAcceptPrivacyNotice={acceptPrivacyNoticeForWalletBackup.trigger}
          onCancelBackup={cancelWalletBackup.trigger}
          onContinue={continueToRecoveryPhraseForWalletBackup.trigger}
        />
      );
    }

    if (currentStep === 'recoveryPhraseDisplay') {
      return (
        <WalletRecoveryPhraseDisplayDialog
          recoveryPhrase={recoveryPhraseWords.reduce((phrase, { word }) => `${phrase} ${word}`, '')}
          onStartWalletBackup={startWalletBackup.trigger}
          onCancelBackup={cancelWalletBackup.trigger}
        />
      );
    }

    if (currentStep === 'recoveryPhraseEntry') {
      return (
        <WalletRecoveryPhraseEntryDialog
          isTermDeviceAccepted={isTermDeviceAccepted}
          enteredPhrase={enteredPhrase}
          canFinishBackup={isRecoveryPhraseValid && isTermDeviceAccepted && isTermRecoveryAccepted}
          isTermRecoveryAccepted={isTermRecoveryAccepted}
          isValid={isRecoveryPhraseValid}
          onAcceptTermDevice={acceptWalletBackupTermDevice.trigger}
          onAcceptTermRecovery={acceptWalletBackupTermRecovery.trigger}
          onAddWord={addWordToWalletBackupVerification.trigger}
          onCancelBackup={cancelWalletBackup.trigger}
          onClear={clearEnteredRecoveryPhrase.trigger}
          onFinishBackup={finishWalletBackup.trigger}
          onRestartBackup={restartWalletBackup.trigger}
          recoveryPhraseShuffled={recoveryPhraseShuffled}
        />
      );
    }
  }
}

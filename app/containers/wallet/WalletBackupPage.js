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
          onAcceptPrivacyNotice={acceptPrivacyNoticeForWalletBackup}
          onCancelBackup={cancelWalletBackup}
          onContinue={continueToRecoveryPhraseForWalletBackup}
        />
      );
    }

    if (currentStep === 'recoveryPhraseDisplay') {
      return (
        <WalletRecoveryPhraseDisplayDialog
          recoveryPhrase={recoveryPhraseWords.reduce((phrase, { word }) => `${phrase} ${word}`, '')}
          onStartWalletBackup={startWalletBackup}
          onCancelBackup={cancelWalletBackup}
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
          onAcceptTermDevice={acceptWalletBackupTermDevice}
          onAcceptTermRecovery={acceptWalletBackupTermRecovery}
          onAddWord={addWordToWalletBackupVerification}
          onCancelBackup={cancelWalletBackup}
          onClear={clearEnteredRecoveryPhrase}
          onFinishBackup={finishWalletBackup}
          onRestartBackup={restartWalletBackup}
          recoveryPhraseShuffled={recoveryPhraseShuffled}
        />
      );
    }
  }
}

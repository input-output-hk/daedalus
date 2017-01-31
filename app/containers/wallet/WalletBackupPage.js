// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletBackupPrivacyWarningDialog from '../../components/wallet/backup-recovery/WalletBackupPrivacyWarningDialog';
import WalletRecoveryPhraseDisplayDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseDisplayDialog';
import WalletRecoveryPhraseEntryDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseEntryDialog';

@inject('stores', 'actions') @observer
export default class WalletBackupPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      walletBackup: PropTypes.shape({
        currentStep: PropTypes.string.isRequired,
        recoveryPhraseWords: MobxPropTypes.arrayOrObservableArray.isRequired,
        recoveryPhraseShuffled: MobxPropTypes.arrayOrObservableArray.isRequired,
        completed: PropTypes.bool.isRequired,
        enteredPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
        isPrivacyNoticeAccepted: PropTypes.bool.isRequired,
        isEntering: PropTypes.bool.isRequired,
        isRecoveryPhraseValid: PropTypes.bool.isRequired,
        countdownRemaining: PropTypes.number.isRequired,
        isTermDeviceAccepted: PropTypes.bool.isRequired,
        isTermRecoveryAccepted: PropTypes.bool.isRequired
      }),
    }).isRequired,
    actions: PropTypes.shape({
      startWalletBackup: PropTypes.func.isRequired,
      continueToRecoveryPhraseForWalletBackup: PropTypes.func.isRequired,
      addWordToWalletBackupVerification: PropTypes.func.isRequired,
      clearEnteredRecoveryPhrase: PropTypes.func.isRequired,
      acceptWalletBackupTermDevice: PropTypes.func.isRequired,
      acceptWalletBackupTermRecovery: PropTypes.func.isRequired,
      restartWalletBackup: PropTypes.func.isRequired,
      cancelWalletBackup: PropTypes.func.isRequired,
      finishWalletBackup: PropTypes.func.isRequired,
      acceptPrivacyNoticeForWalletBackup: PropTypes.func.isRequired
    }).isRequired
  };

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
    } = this.props.actions;
    if (currentStep === 'privacyWarning') return (
      <WalletBackupPrivacyWarningDialog
        canPhraseBeShown={isPrivacyNoticeAccepted && countdownRemaining === 0}
        isPrivacyNoticeAccepted={isPrivacyNoticeAccepted}
        countdownRemaining={countdownRemaining}
        onAcceptPrivacyNotice={acceptPrivacyNoticeForWalletBackup}
        onCancelBackup={cancelWalletBackup}
        onContinue={continueToRecoveryPhraseForWalletBackup}
      />
    );
    if (currentStep === 'recoveryPhraseDisplay') return (
      <WalletRecoveryPhraseDisplayDialog
        recoveryPhrase={recoveryPhraseWords.reduce((phrase, { word }) => `${phrase} ${word}`, '')}
        onStartWalletBackup={startWalletBackup}
        onCancelBackup={cancelWalletBackup}
      />
    );
    if (currentStep === 'recoveryPhraseEntry') return (
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

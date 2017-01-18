// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletRecoveryPhraseDialog from '../../components/wallet/backup-recovery/WalletRecoveryPhraseDialog';

@inject('stores', 'actions') @observer
export default class WalletBackupPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      walletBackup: PropTypes.shape({
        walletId: PropTypes.string.isRequired,
        recoveryPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
        recoveryPhraseShuffled: MobxPropTypes.arrayOrObservableArray.isRequired,
        completed: PropTypes.bool.isRequired,
        enteredPhrase: MobxPropTypes.arrayOrObservableArray.isRequired,
        isPrivacyNoticeAccepted: PropTypes.bool.isRequired,
        isEntering: PropTypes.bool.isRequired,
        isRecoveryPhraseValid: PropTypes.bool.isRequired,
        isWalletBackupStartAccepted: PropTypes.bool.isRequired,
        countdownRemaining: PropTypes.number.isRequired,
        isTermDeviceAccepted: PropTypes.bool.isRequired,
        isTermRecoveryAccepted: PropTypes.bool.isRequired
      }),
    }).isRequired,
    actions: PropTypes.shape({
      acceptWalletBackupStart: PropTypes.func.isRequired,
      startWalletBackup: PropTypes.func.isRequired,
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
      recoveryPhrase,
      enteredPhrase,
      isEntering,
      isRecoveryPhraseValid,
      isWalletBackupStartAccepted,
      countdownRemaining,
      recoveryPhraseShuffled,
      isTermDeviceAccepted,
      isTermRecoveryAccepted,
      isPrivacyNoticeAccepted
    } = this.props.stores.walletBackup;
    const {
      acceptWalletBackupStart,
      startWalletBackup,
      addWordToWalletBackupVerification,
      clearEnteredRecoveryPhrase,
      acceptWalletBackupTermDevice,
      acceptWalletBackupTermRecovery,
      restartWalletBackup,
      cancelWalletBackup,
      finishWalletBackup,
      acceptPrivacyNoticeForWalletBackup
    } = this.props.actions;
    return (
      <WalletRecoveryPhraseDialog
        enteredPhrase={enteredPhrase}
        isEntering={isEntering}
        isValid={isRecoveryPhraseValid}
        recoveryPhrase={recoveryPhrase}
        recoveryPhraseShuffled={recoveryPhraseShuffled}
        isWalletBackupStartAccepted={isWalletBackupStartAccepted}
        onAcceptStartBackup={acceptWalletBackupStart}
        countdownRemaining={countdownRemaining}
        canPhraseBeShown={countdownRemaining === 0 && isPrivacyNoticeAccepted}
        onStartWalletBackup={startWalletBackup}
        onAddWord={addWordToWalletBackupVerification}
        onClear={clearEnteredRecoveryPhrase}
        onAcceptTermDevice={acceptWalletBackupTermDevice}
        onAcceptTermRecovery={acceptWalletBackupTermRecovery}
        isTermDeviceAccepted={isTermDeviceAccepted}
        isTermRecoveryAccepted={isTermRecoveryAccepted}
        canFinishBackup={isTermDeviceAccepted && isTermRecoveryAccepted && isRecoveryPhraseValid}
        onRestartBackup={restartWalletBackup}
        onCancelBackup={cancelWalletBackup}
        onFinishBackup={finishWalletBackup}
        onAcceptPrivacyNotice={acceptPrivacyNoticeForWalletBackup}
        isPrivacyNoticeAccepted={isPrivacyNoticeAccepted}
      />
    );
  }

}

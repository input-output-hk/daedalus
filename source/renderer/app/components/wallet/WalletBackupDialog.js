// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletBackupPrivacyWarningDialog from './backup-recovery/WalletBackupPrivacyWarningDialog';
import WalletRecoveryPhraseDisplayDialog from './backup-recovery/WalletRecoveryPhraseDisplayDialog';
import WalletRecoveryPhraseEntryDialog from './backup-recovery/WalletRecoveryPhraseEntryDialog';
import type {
  RecoveryPhraseWord,
  walletBackupStep,
} from '../../types/walletBackupTypes';
import { WALLET_BACKUP_STEPS } from '../../types/walletBackupTypes';

type Props = {
  currentStep: walletBackupStep,
  canPhraseBeShown: boolean,
  isPrivacyNoticeAccepted: boolean,
  countdownRemaining: number,
  isTermOfflineAccepted: boolean,
  canFinishBackup: boolean,
  isTermRecoveryAccepted: boolean,
  isTermRewardsAccepted: boolean,
  isValid: boolean,
  isSubmitting: boolean,
  recoveryPhrase: string,
  recoveryPhraseShuffled: Array<RecoveryPhraseWord>,
  enteredPhrase: Array<{ word: string }>,
  onCancelBackup: Function,
  onAcceptPrivacyNotice: Function,
  onContinue: Function,
  onStartWalletBackup: Function,
  onAcceptTermOffline: Function,
  onAcceptTermRecovery: Function,
  onAcceptTermRewards: Function,
  onAddWord: Function,
  onClear: Function,
  onFinishBackup: Function,
  onRestartBackup: Function,
};

@observer
export default class WalletBackupDialog extends Component<Props> {
  render() {
    const {
      currentStep,
      onCancelBackup,
      canPhraseBeShown,
      isPrivacyNoticeAccepted,
      countdownRemaining,
      onAcceptPrivacyNotice,
      onContinue,
      recoveryPhrase,
      onStartWalletBackup,
      isTermOfflineAccepted,
      enteredPhrase,
      canFinishBackup,
      isTermRecoveryAccepted,
      isTermRewardsAccepted,
      isValid,
      isSubmitting,
      onAcceptTermOffline,
      onAcceptTermRecovery,
      onAcceptTermRewards,
      onAddWord,
      onClear,
      onFinishBackup,
      onRestartBackup,
      recoveryPhraseShuffled,
    } = this.props;

    if (currentStep === WALLET_BACKUP_STEPS.PRIVACY_WARNING) {
      return (
        <WalletBackupPrivacyWarningDialog
          canPhraseBeShown={canPhraseBeShown}
          isPrivacyNoticeAccepted={isPrivacyNoticeAccepted}
          countdownRemaining={countdownRemaining}
          onAcceptPrivacyNotice={onAcceptPrivacyNotice}
          onCancelBackup={onCancelBackup}
          onContinue={onContinue}
        />
      );
    }
    if (currentStep === WALLET_BACKUP_STEPS.RECOVERY_PHRASE_DISPLAY) {
      return (
        <WalletRecoveryPhraseDisplayDialog
          recoveryPhrase={recoveryPhrase}
          onStartWalletBackup={onStartWalletBackup}
          onCancelBackup={onCancelBackup}
        />
      );
    }
    if (currentStep === WALLET_BACKUP_STEPS.RECOVERY_PHRASE_ENTRY) {
      return (
        <WalletRecoveryPhraseEntryDialog
          isTermOfflineAccepted={isTermOfflineAccepted}
          enteredPhrase={enteredPhrase}
          canFinishBackup={canFinishBackup}
          isTermRecoveryAccepted={isTermRecoveryAccepted}
          isTermRewardsAccepted={isTermRewardsAccepted}
          isValid={isValid}
          isSubmitting={isSubmitting}
          onAcceptTermOffline={onAcceptTermOffline}
          onAcceptTermRecovery={onAcceptTermRecovery}
          onAcceptTermRewards={onAcceptTermRewards}
          onAddWord={onAddWord}
          onCancelBackup={onCancelBackup}
          onClear={onClear}
          onFinishBackup={onFinishBackup}
          onRestartBackup={onRestartBackup}
          recoveryPhraseShuffled={recoveryPhraseShuffled}
        />
      );
    }
    return null;
  }
}

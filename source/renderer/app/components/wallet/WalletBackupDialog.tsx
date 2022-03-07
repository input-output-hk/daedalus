import React, { Component } from 'react';
import { observer } from 'mobx-react';
import WalletBackupPrivacyWarningDialog from './backup-recovery/WalletBackupPrivacyWarningDialog';
import WalletRecoveryPhraseDisplayDialog from './backup-recovery/WalletRecoveryPhraseDisplayDialog';
import WalletRecoveryPhraseEntryDialog from './backup-recovery/WalletRecoveryPhraseEntryDialog';
import type { walletBackupStep } from '../../types/walletBackupTypes';
import { WALLET_BACKUP_STEPS } from '../../types/walletBackupTypes';

type Props = {
  currentStep: walletBackupStep;
  canPhraseBeShown: boolean;
  isPrivacyNoticeAccepted: boolean;
  countdownRemaining: number;
  isTermOfflineAccepted: boolean;
  canFinishBackup: boolean;
  isTermRecoveryAccepted: boolean;
  isValid: boolean;
  isSubmitting: boolean;
  recoveryPhrase: string;
  enteredPhrase: Array<string>;
  onCancelBackup: (...args: Array<any>) => any;
  onAcceptPrivacyNotice: (...args: Array<any>) => any;
  onContinue: (...args: Array<any>) => any;
  onStartWalletBackup: (...args: Array<any>) => any;
  onAcceptTermOffline: (...args: Array<any>) => any;
  onAcceptTermRecovery: (...args: Array<any>) => any;
  onUpdateVerificationPhrase: (...args: Array<any>) => any;
  onFinishBackup: (...args: Array<any>) => any;
  onRestartBackup: (...args: Array<any>) => any;
};

@observer
class WalletBackupDialog extends Component<Props> {
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
      isValid,
      isSubmitting,
      onAcceptTermOffline,
      onAcceptTermRecovery,
      onUpdateVerificationPhrase,
      onFinishBackup,
      onRestartBackup,
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
          isSubmitting={isSubmitting}
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
          isValid={isValid}
          isSubmitting={isSubmitting}
          onAcceptTermOffline={onAcceptTermOffline}
          onAcceptTermRecovery={onAcceptTermRecovery}
          onUpdateVerificationPhrase={onUpdateVerificationPhrase}
          onCancelBackup={onCancelBackup}
          onFinishBackup={onFinishBackup}
          onRestartBackup={onRestartBackup}
        />
      );
    }

    return null;
  }
}

export default WalletBackupDialog;

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletBackupDialog from '../../../components/wallet/WalletBackupDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletBackupDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onCancelBackup = () => {
    this.props.onClose();
    this.props.actions.walletBackup.cancelWalletBackup.trigger();
  };

  render() {
    const { actions, stores } = this.props;
    const {
      enteredPhrase,
      isRecoveryPhraseValid,
      countdownRemaining,
      isTermOfflineAccepted,
      isTermRecoveryAccepted,
      isTermRewardsAccepted,
      isPrivacyNoticeAccepted,
      currentStep,
      recoveryPhrase,
    } = stores.walletBackup;
    const {
      startWalletBackup,
      updateWalletBackupVerificationPhrase,
      acceptWalletBackupTermOffline,
      acceptWalletBackupTermRecovery,
      acceptWalletBackupTermRewards,
      restartWalletBackup,
      finishWalletBackup,
      acceptPrivacyNoticeForWalletBackup,
      continueToRecoveryPhraseForWalletBackup,
    } = actions.walletBackup;
    const { createWalletRequest } = stores.wallets;
    const { isShelleyActivated } = stores.networkStatus;

    const canFinishBackup = global.isIncentivizedTestnet
      ? isRecoveryPhraseValid &&
        isTermOfflineAccepted &&
        isTermRecoveryAccepted &&
        isTermRewardsAccepted
      : isRecoveryPhraseValid &&
        isTermOfflineAccepted &&
        isTermRecoveryAccepted;
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
        recoveryPhrase={recoveryPhrase.join(' ')}
        onStartWalletBackup={startWalletBackup.trigger}
        // Props for WalletRecoveryPhraseEntryDialog
        isTermOfflineAccepted={isTermOfflineAccepted}
        enteredPhrase={enteredPhrase}
        canFinishBackup={canFinishBackup}
        isTermRecoveryAccepted={isTermRecoveryAccepted}
        isTermRewardsAccepted={isTermRewardsAccepted}
        isValid={isRecoveryPhraseValid}
        isSubmitting={createWalletRequest.isExecuting}
        onAcceptTermOffline={acceptWalletBackupTermOffline.trigger}
        onAcceptTermRecovery={acceptWalletBackupTermRecovery.trigger}
        onAcceptTermRewards={acceptWalletBackupTermRewards.trigger}
        onUpdateVerificationPhrase={
          updateWalletBackupVerificationPhrase.trigger
        }
        onFinishBackup={() => {
          finishWalletBackup.trigger();
        }}
        onRestartBackup={restartWalletBackup.trigger}
        isShelleyActivated={isShelleyActivated}
      />
    );
  }
}

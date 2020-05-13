// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep1Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import {
  LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT,
  WALLET_RECOVERY_PHRASE_WORD_COUNT,
} from '../../../../config/cryptoConfig';
import type { InjectedProps as Props } from '../../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  get step() {
    return this.props.stores.walletSettings.recoveryPhraseStep;
  }

  get recoveryPhraseComponent() {
    const { step } = this;
    if (step === 1) return WalletRecoveryPhraseStep1Dialog;
    if (step === 2) return WalletRecoveryPhraseStep2Dialog;
    if (step === 3) return WalletRecoveryPhraseStep3Dialog;
    if (step === 4) return WalletRecoveryPhraseStep4Dialog;
    return null;
  }

  render() {
    const { stores, actions } = this.props;
    const { active: activeWallet } = stores.wallets;
    if (!activeWallet) throw new Error('Active wallet required.');
    const {
      recoveryPhraseVerificationContinue,
      recoveryPhraseVerificationCheck,
      recoveryPhraseVerificationClose,
    } = actions.walletSettings;
    const { openExternalLink } = stores.app;
    const onContinue =
      this.step === 2
        ? recoveryPhraseVerificationCheck.trigger
        : recoveryPhraseVerificationContinue.trigger;
    const onClose = recoveryPhraseVerificationClose.trigger;

    const wordCount = activeWallet.isRandom
      ? LEGACY_WALLET_RECOVERY_PHRASE_WORD_COUNT
      : WALLET_RECOVERY_PHRASE_WORD_COUNT;
    const WalletRecoveryPhraseDialog = this.recoveryPhraseComponent;
    return (
      WalletRecoveryPhraseDialog && (
        <WalletRecoveryPhraseDialog
          onContinue={onContinue}
          onClose={onClose}
          wordCount={wordCount}
          openExternalLink={openExternalLink}
        />
      )
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import validWords from '../../../../../../common/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../../common/crypto/decrypt';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseStep2Container extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  componentWillReceiveProps(nextProps: Props) {
    const { walletBackup } = nextProps.stores;
    const { actions } = this.props;
    const {
      isRecoveryPhraseMatching: nextRecoveryPhraseMatching,
      getWalletIdAndBalanceRequest: getWalletIdAndBalanceRequestNext,
    } = walletBackup;

    let dialog;
    if (getWalletIdAndBalanceRequestNext.wasExecuted) {
      if (nextRecoveryPhraseMatching) {
        dialog = WalletRecoveryPhraseStep3Dialog;
        actions.wallets.updateRecoveryPhraseVerificationDate.trigger();
      } else {
        dialog = WalletRecoveryPhraseStep4Dialog;
      }
      actions.dialogs.open.trigger({
        dialog,
      });
      actions.walletBackup.resetRecoveryPhraseCheck.trigger();
    }
  }

  handleVerify = (recoveryPhrase1: Array<string>) => {
    const recoveryPhrase = [
      'arctic',
      'decade',
      'pink',
      'easy',
      'jar',
      'index',
      'base',
      'bright',
      'vast',
      'ocean',
      'hard',
      'pizza',
    ];
    this.props.actions.walletBackup.checkRecoveryPhrase.trigger({
      recoveryPhrase,
    });
  };

  render() {
    const { stores } = this.props;
    const { walletBackup } = stores;
    const { getWalletIdAndBalanceRequest } = walletBackup;
    const { closeActiveDialog } = this.props.actions.dialogs;

    const isVerifying =
      getWalletIdAndBalanceRequest.isExecuting ||
      getWalletIdAndBalanceRequest.wasExecuted;

    return (
      <WalletRecoveryPhraseStep2Dialog
        mnemonicValidator={isValidMnemonic}
        suggestedMnemonics={validWords}
        isVerifying={isVerifying}
        onVerify={this.handleVerify}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

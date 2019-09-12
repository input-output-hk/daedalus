// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
import validWords from '../../../../../../common/crypto/valid-words.en';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

type State = {
  isVerifying: boolean,
};

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseStep2Container extends Component<
  Props,
  State
> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  state = {
    isVerifying: false,
  };

  componentWillReceiveProps(nextProps: Props) {
    const { walletBackup, wallets } = nextProps.stores;
    const { actions } = this.props;
    const {
      isRecoveryPhraseMatching: nextRecoveryPhraseMatching,
      getWalletIdAndBalanceRequest: getWalletIdAndBalanceRequestNext,
    } = walletBackup;

    let dialog;
    if (getWalletIdAndBalanceRequestNext.wasExecuted) {
      if (nextRecoveryPhraseMatching) {
        dialog = WalletRecoveryPhraseStep3Dialog;
        const activeWallet = wallets.active;
        if (activeWallet) activeWallet.updateWalletLocalData();
      } else {
        dialog = WalletRecoveryPhraseStep4Dialog;
      }
      this.setState({ isVerifying: false });
      actions.dialogs.open.trigger({
        dialog,
      });
      actions.walletBackup.resetRecoveryPhraseCheck.trigger();
    }
  }

  handleVerify = (recoveryPhrase: Array<string>) => {
    this.setState({ isVerifying: true });
    this.props.actions.walletBackup.checkRecoveryPhrase.trigger({
      recoveryPhrase,
    });
  };

  render() {
    const { isVerifying } = this.state;
    const { isValidMnemonic } = this.props.stores.wallets;
    const { closeActiveDialog } = this.props.actions.dialogs;

    return (
      <WalletRecoveryPhraseStep2Dialog
        mnemonicValidator={mnemonic => isValidMnemonic(mnemonic)}
        suggestedMnemonics={validWords}
        isVerifying={isVerifying}
        onVerify={this.handleVerify}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

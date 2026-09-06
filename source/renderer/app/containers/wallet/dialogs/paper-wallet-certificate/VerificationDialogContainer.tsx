import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import validWords from '../../../../../../common/config/crypto/valid-words.en';
import VerificationDialog from '../../../../components/wallet/paper-wallet-certificate/VerificationDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class VerificationDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };
  onContinue = () => {
    this.props.actions.wallets.updateCertificateStep.trigger();
  };

  render() {
    const { wallets } = this.props.stores;
    const {
      walletCertificateRecoveryPhrase,
      additionalMnemonicWords,
    } = wallets;

    if (!walletCertificateRecoveryPhrase || !additionalMnemonicWords) {
      throw new Error(
        'Props walletCertificateRecoveryPhrase and additionalMnemonicWords are required'
      );
    }

    return (
      <VerificationDialog
        suggestedMnemonics={validWords}
        additionalMnemonicWords={additionalMnemonicWords}
        walletCertificateRecoveryPhrase={walletCertificateRecoveryPhrase}
        onContinue={this.onContinue}
        onClose={this.props.onClose}
      />
    );
  }
}

export default VerificationDialogContainer;

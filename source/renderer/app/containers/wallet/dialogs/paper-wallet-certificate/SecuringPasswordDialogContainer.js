// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SecuringPasswordDialog from '../../../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class SecuringPasswordDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onContinue = () => {
    this.props.actions.ada.wallets.updateCertificateStep.trigger();
  };

  render() {
    const { wallets } = this.props.stores.ada;
    const {
      additionalMnemonicWords,
      walletCertificateAddress,
      walletCertificateRecoveryPhrase,
    } = wallets;

    return (
      <SecuringPasswordDialog
        additionalMnemonics={additionalMnemonicWords}
        walletCertificateAddress={walletCertificateAddress}
        walletCertificateRecoveryPhrase={walletCertificateRecoveryPhrase}
        onContinue={this.onContinue}
      />
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SecuringPasswordDialog from '../../../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

@inject('stores', 'actions') @observer
export default class SecuringPasswordDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onContinue = () => {
    this.props.actions.ada.wallets.updateCertificateStep.trigger();
  };

  render() {
    const { stores } = this.props;
    const { wallets } = stores.ada;
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

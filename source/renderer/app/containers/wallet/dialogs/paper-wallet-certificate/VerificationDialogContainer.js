// @flow
import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer, inject } from 'mobx-react';
import validWords from '../../../../../../common/valid-words.en';
import VerificationDialog from '../../../../components/wallet/paper-wallet-certificate/VerificationDialog';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

@inject('stores', 'actions') @observer
export default class VerificationDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onContinue = (values: { recoveryPhrase: Array<string>, password: string }) => {
    this.props.actions.ada.wallets.verifyCertificate.trigger(values);
  };

  render() {
    const { wallets } = this.props.stores.ada;
    const {
      walletCertificatePassword,
      walletCertificateRecoveryPhrase,
      walletCertificateHasError,
    } = wallets;

    return (
      <VerificationDialog
        suggestedMnemonics={validWords}
        walletCertificateRecoveryPhrase={toJS(walletCertificateRecoveryPhrase)}
        walletCertificatePassword={walletCertificatePassword}
        onContinue={this.onContinue}
        error={walletCertificateHasError}
      />
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import CompletionDialog from '../../../../components/wallet/paper-wallet-certificate/CompletionDialog';
import type { StoresMap } from '../../../../stores/index';

type Props = {
  stores: any | StoresMap,
  onFinish: Function,
};

@inject('stores') @observer
export default class CompletionDialogContainer extends Component<Props> {

  static defaultProps = { stores: null };

  render() {
    const { walletCertificateAddress } = this.props.stores.ada.wallets;

    return (
      <CompletionDialog
        walletCertificateAddress={walletCertificateAddress}
        onFinish={this.props.onFinish}
      />
    );
  }
}

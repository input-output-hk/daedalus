// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import PaperWallet from '../../components/wallet/PaperWallet';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class PaperWalletPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const { wallets } = this.props.stores;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for PaperWalletPage.');

    return (
      <PaperWallet
        walletName={wallet.name}
        walletAddress={wallet.address}
      />
    );
  }

}

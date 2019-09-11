// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
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

  handleContinue = () => {};

  render() {
    // const { wallets, walletSettings } = this.props.stores;
    // const activeWallet = wallets.active;
    // const { exportWalletToFileRequest } = walletSettings;

    return <WalletRecoveryPhraseStep2Dialog onContinue={this.handleContinue} />;
  }
}

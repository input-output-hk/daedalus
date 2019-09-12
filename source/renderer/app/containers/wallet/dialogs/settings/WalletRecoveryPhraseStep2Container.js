// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import WalletRecoveryPhraseStep3Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep3Dialog';
import WalletRecoveryPhraseStep4Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep4Dialog';
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

  updateWalletLocalData = () => {
    const { wallets } = this.props.stores;
    const activeWallet = wallets.active;
    activeWallet.updateWalletLocalData();
  };

  handleVerify = successful => {
    const dialog = successful
      ? WalletRecoveryPhraseStep3Dialog
      : WalletRecoveryPhraseStep4Dialog;
    if (successful) this.updateWalletLocalData();
    this.props.actions.dialogs.open.trigger({
      dialog,
    });
  };

  render() {
    const { closeActiveDialog } = this.props.actions.dialogs;
    return (
      <WalletRecoveryPhraseStep2Dialog
        onVerify={this.handleVerify}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

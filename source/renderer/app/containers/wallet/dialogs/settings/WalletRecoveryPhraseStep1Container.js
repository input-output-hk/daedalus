// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep1Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep1Dialog';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletRecoveryPhraseStep1Container extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  handleContinue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletRecoveryPhraseStep2Dialog,
    });
  };

  render() {
    const { stores, actions } = this.props;
    const { closeActiveDialog } = actions.dialogs;
    const { active: activeWallet } = stores.wallets;
    if (!activeWallet) throw new Error('Active wallet required.');
    const wordCount = activeWallet.discovery === 'random' ? 12 : 15;
    return (
      <WalletRecoveryPhraseStep1Dialog
        onContinue={this.handleContinue}
        onClose={closeActiveDialog.trigger}
        wordCount={wordCount}
      />
    );
  }
}

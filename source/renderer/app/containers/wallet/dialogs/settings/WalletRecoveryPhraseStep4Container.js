// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletRecoveryPhraseStep2Dialog from '../../../../components/wallet/settings/WalletRecoveryPhraseStep2Dialog';
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

  handleVerify = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletRecoveryPhraseStep2Dialog,
    });
  };

  render() {
    const { closeActiveDialog } = this.props.actions.dialogs;
    return (
      <WalletRecoveryPhraseStep4Dialog
        onVerifyAgain={this.handleVerify}
        onClose={closeActiveDialog.trigger}
      />
    );
  }
}

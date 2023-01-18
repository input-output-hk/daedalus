import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SecuringPasswordDialog from '../../../../components/wallet/paper-wallet-certificate/SecuringPasswordDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class SecuringPasswordDialogContainer extends Component<Props> {
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
    const { additionalMnemonicWords } = wallets;

    if (!additionalMnemonicWords) {
      throw new Error('Prop additionalMnemonicWords is required but was null.');
    }

    return (
      <SecuringPasswordDialog
        additionalMnemonics={additionalMnemonicWords}
        onContinue={this.onContinue}
        onClose={this.props.onClose}
      />
    );
  }
}

export default SecuringPasswordDialogContainer;

// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import MnemonicsDialog from '../../../../components/wallet/wallet-restore/MnemonicsDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class MnemonicsDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  handleSetWalletMnemonics = (mnemonics: Array<string>) =>
    this.props.actions.wallets.restoreWalletSetMnemonics.trigger({ mnemonics });

  render() {
    const { onContinue, onClose, onBack, stores } = this.props;
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
      mnemonics,
    } = stores.wallets;
    return (
      <MnemonicsDialog
        onClose={onClose}
        onContinue={onContinue}
        onBack={onBack}
        walletKind={walletKind}
        walletKindDaedalus={walletKindDaedalus}
        walletKindYoroi={walletKindYoroi}
        walletKindHardware={walletKindHardware}
        onSetWalletMnemonics={this.handleSetWalletMnemonics}
        mnemonics={mnemonics}
      />
    );
  }
}

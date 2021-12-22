import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletTypeDialog from '../../../../components/wallet/wallet-restore/WalletTypeDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class WalletTypeDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;
  handleSetWalletKind = (kind: string, param?: string) =>
    this.props.actions.wallets.restoreWalletSetKind.trigger({
      param,
      kind,
    });

  render() {
    const { onClose, onContinue, stores } = this.props;
    const {
      walletKind,
      walletKindDaedalus,
      walletKindYoroi,
      walletKindHardware,
    } = stores.wallets;
    return (
      <WalletTypeDialog
        onClose={onClose}
        onContinue={onContinue}
        walletKind={walletKind}
        walletKindDaedalus={walletKindDaedalus}
        walletKindYoroi={walletKindYoroi}
        walletKindHardware={walletKindHardware}
        onSetWalletKind={this.handleSetWalletKind}
      />
    );
  }
}

export default WalletTypeDialogContainer;

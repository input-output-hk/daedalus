// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import TransferFundsStep1Dialog from '../../../../components/wallet/transfer-funds/TransferFundsStep1Dialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class TransferFundsStep1Container extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, actions, onClose, onContinue } = this.props;
    const {
      transferFundsWalletFromId,
      transferFundsWalletToId,
      allLegacyWallets,
      allWallets,
    } = stores.wallets;
    const { transferFundsSetWalletToId } = actions.wallets;
    const walletFrom = allLegacyWallets.find(
      ({ id }) => id === transferFundsWalletFromId
    );
    if (!walletFrom) return null;
    return (
      <TransferFundsStep1Dialog
        walletToId={transferFundsWalletToId}
        walletFrom={walletFrom}
        wallets={allWallets}
        onClose={onClose}
        onContinue={onContinue}
        onSetToWallet={(walletToId: string) =>
          transferFundsSetWalletToId.trigger({ walletToId })
        }
      />
    );
  }
}

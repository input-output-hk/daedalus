// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletPublicKeyQRCodeDialog from '../../../../components/wallet/settings/WalletPublicKeyQRCodeDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class WalletPublicKeyQRCodeDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { wallets } = stores;
    const {
      active: activeWallet,
      activePublicKey: activeWalletPublicKey,
    } = wallets;

    if (!activeWallet)
      throw new Error(
        'Active wallet required for WalletPublicKeyQRCodeDialogContainer.'
      );

    if (!activeWalletPublicKey)
      throw new Error(
        'Active wallet public key required for WalletPublicKeyQRCodeDialogContainer.'
      );

    return (
      <WalletPublicKeyQRCodeDialog
        walletName={activeWallet.name}
        walletPublicKey={activeWalletPublicKey}
        onClose={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
      />
    );
  }
}

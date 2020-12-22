// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletPublicKeyQRCodeDialog from '../../../../components/wallet/settings/WalletPublicKeyQRCodeDialog';
import { ellipsis } from '../../../../utils/strings';
import { WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH } from '../../../../config/walletsConfig';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class WalletPublicKeyQRCodeDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleCopyWalletPublicKey = () => {
    const { actions, stores } = this.props;
    const { wallets: walletsAction } = actions;
    const { wallets: walletsStore } = stores;
    const { activePublicKey } = walletsStore;

    if (!activePublicKey)
      throw new Error(
        'Active wallet public key required for WalletPublicKeyQRCodeDialogContainer.'
      );

    const publicKey = ellipsis(
      activePublicKey,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );

    walletsAction.copyPublicKey.trigger({ publicKey });
  };

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
        onCopyWalletPublicKey={this.handleCopyWalletPublicKey}
        onClose={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
      />
    );
  }
}

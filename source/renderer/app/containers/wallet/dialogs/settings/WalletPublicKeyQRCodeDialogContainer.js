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
    const { walletsPublicKeys, active } = walletsStore;

    if (!active || !walletsPublicKeys[active.id])
      throw new Error(
        'Active wallet public key required for WalletPublicKeyQRCodeDialogContainer.'
      );

    const publicKey = ellipsis(
      walletsPublicKeys[active.id],
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );

    walletsAction.copyPublicKey.trigger({ publicKey });
  };

  render() {
    const { actions, stores } = this.props;
    const { wallets } = stores;
    const { active: activeWallet, walletsPublicKeys } = wallets;

    if (!activeWallet)
      throw new Error(
        'Active wallet required for WalletPublicKeyQRCodeDialogContainer.'
      );

    if (!walletsPublicKeys[activeWallet.id])
      throw new Error(
        'Active wallet public key required for WalletPublicKeyQRCodeDialogContainer.'
      );

    return (
      <WalletPublicKeyQRCodeDialog
        walletName={activeWallet.name}
        walletPublicKey={walletsPublicKeys[activeWallet.id]}
        onCopyWalletPublicKey={this.handleCopyWalletPublicKey}
        onClose={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
      />
    );
  }
}

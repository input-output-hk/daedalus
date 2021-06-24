// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import PublicKeyQRCodeDialog from '../../../../components/wallet/settings/PublicKeyQRCodeDialog';
import type { $npm$ReactIntl$MessageDescriptor } from '../../../../components/wallet/settings/PublicKeyQRCodeDialog';
import { ellipsis } from '../../../../utils/strings';
import { WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH } from '../../../../config/walletsConfig';
import type { InjectedProps } from '../../../../types/injectedPropsType';

const walletMessages: {
  [string]: $npm$ReactIntl$MessageDescriptor,
} = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet Public Key',
    description: 'Title for the "Wallet Public Key QR Code" dialog.',
  },
  copyPublicKeyLabel: {
    id: 'wallet.settings.copyPublicKey',
    defaultMessage: '!!!Copy public key',
    description: 'Copy public key label.',
  },
});

const icoMessages: {
  [string]: $npm$ReactIntl$MessageDescriptor,
} = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.icoPublicKey',
    defaultMessage: '!!!ICO Public Key',
    description: 'Title for the "ICO Public Key QR Code" dialog.',
  },
  copyPublicKeyLabel: {
    id: 'wallet.settings.copyPublicKey',
    defaultMessage: '!!!Copy public key',
    description: 'Copy public key label.',
  },
});

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class PublicKeyQRCodeDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleCopyWalletPublicKey = () => {
    const { actions, stores } = this.props;
    const { wallets: walletsAction } = actions;
    const { wallets: walletsStore } = stores;
    const { activePublicKey } = walletsStore;

    if (!activePublicKey)
      throw new Error(
        'Active wallet public key required for PublicKeyQRCodeDialogContainer.'
      );

    const publicKey = ellipsis(
      activePublicKey,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH,
      WALLET_PUBLIC_KEY_NOTIFICATION_SEGMENT_LENGTH
    );

    walletsAction.copyPublicKey.trigger({ publicKey });
  };

  render() {
    const { actions, stores, isICO = false } = this.props;
    const { wallets } = stores;
    const { active: activeWallet, activePublicKey, icoPublicKey } = wallets;

    if (!activeWallet)
      throw new Error(
        'Active wallet required for PublicKeyQRCodeDialogContainer.'
      );

    if (!icoPublicKey && !activePublicKey) {
      throw new Error(
        'Active wallet public key or ICO public key required for PublicKeyQRCodeDialogContainer.'
      );
    }

    if (isICO && !!icoPublicKey) {
      return (
        <PublicKeyQRCodeDialog
          walletName={activeWallet.name}
          walletPublicKey={icoPublicKey}
          onCopyWalletPublicKey={this.handleCopyWalletPublicKey}
          onClose={() => {
            actions.dialogs.closeActiveDialog.trigger();
          }}
          messages={icoMessages}
        />
      );
    }
    if (!isICO && !!activePublicKey) {
      return (
        <PublicKeyQRCodeDialog
          walletName={activeWallet.name}
          walletPublicKey={activePublicKey}
          onCopyWalletPublicKey={this.handleCopyWalletPublicKey}
          onClose={() => {
            actions.dialogs.closeActiveDialog.trigger();
          }}
          messages={walletMessages}
        />
      );
    }

    return null;
  }
}

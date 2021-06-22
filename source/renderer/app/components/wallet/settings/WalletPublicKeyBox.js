// @flow
import React from 'react';
import type { Node } from 'react';
import { defineMessages } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import PublicKeyQRCodeDialog from './WalletPublicKeyQRCodeDialog';
import PublicKeyDialog from './WalletPublicKeyDialog';
import type { Locale } from '../../../../../common/types/locales.types';
import GenericPublicKeyField from './GenericPublicKeyField';

export const messages = defineMessages({
  publicKey: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet public key',
    description: 'Wallet public key label.',
  },
  publicKeyShowInstruction: {
    id: 'wallet.settings.walletPublicKeyShowInstruction',
    defaultMessage:
      '!!!Click Reveal on the right-hand side to display the public key of the wallet.',
    description: 'Wallet public key show instruction.',
  },
  showQRCode: {
    id: 'wallet.settings.showQRCode',
    defaultMessage: '!!!Show QR code',
    description: 'Show QR code tooltip.',
  },
});

type Props = {
  publicKey: ?string,
  locale: Locale,
  onCopyWalletPublicKey: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
  publicKeyDialogContainer: Node,
  publicKeyQRCodeDialogContainer: Node,
  t: Function,
};

export const WalletPublicKeyBox = (props: Props) => {
  const {
    publicKey,
    locale,
    onCopyWalletPublicKey,
    openDialogAction,
    isDialogOpen,
    publicKeyDialogContainer,
    publicKeyQRCodeDialogContainer,
    t,
  } = props;

  return (
    <>
      <BorderedBox className={styles.walletPublicKeyBox}>
        <GenericPublicKeyField
          publicKey={publicKey || ''}
          locale={locale}
          onCopyPublicKey={onCopyWalletPublicKey}
          onShowQRCode={() =>
            openDialogAction({ dialog: PublicKeyQRCodeDialog })
          }
          onOpenWalletKeyDialog={() =>
            openDialogAction({ dialog: PublicKeyDialog })
          }
          t={t}
          messages={messages}
        />
      </BorderedBox>
      {isDialogOpen(PublicKeyDialog) ? publicKeyDialogContainer : false}
      {isDialogOpen(PublicKeyQRCodeDialog)
        ? publicKeyQRCodeDialogContainer
        : false}
    </>
  );
};

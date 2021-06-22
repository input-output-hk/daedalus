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
    id: 'wallet.settings.ICOPublicKey',
    defaultMessage: '!!!ICO public key',
    description: 'Wallet public key label.',
  },
  publicKeyDescription: {
    id: 'wallet.settings.ICOPublicKey.description',
    defaultMessage:
      "!!!Your wallet's ICO public key enables participation in the initial coin offering presales.",
    description: 'ICO public key header on the wallet settings page.',
  },
  publicKeyShowInstruction: {
    id: 'wallet.settings.ICOPublicKeyShowInstruction',
    defaultMessage:
      '!!!Click the icon on the right to view your ICO public key.',
    description: 'ICO public key show instruction.',
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

export const ICOPublicKeyBox = (props: Props) => {
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
          description={t(messages.publicKeyDescription)}
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

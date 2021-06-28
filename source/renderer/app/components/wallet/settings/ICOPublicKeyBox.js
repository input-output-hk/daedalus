// @flow
import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSettings.scss';
import PublicKeyQRCodeDialog from './ICOPublicKeyQRCodeDialog';
import ICOPublicKeyDialog from './ICOPublicKeyDialog';
import type { Locale } from '../../../../../common/types/locales.types';
import PublicKeyField from './PublicKeyField';

export const messages = defineMessages({
  publicKey: {
    id: 'wallet.settings.icoPublicKey',
    defaultMessage: '!!!ICO public key',
    description: 'Wallet public key label.',
  },
  publicKeyDescription: {
    id: 'wallet.settings.icoPublicKey.description',
    defaultMessage:
      "!!!Your wallet's ICO public key enables participation in the initial coin offering presales.",
    description: 'ICO public key header on the wallet settings page.',
  },
  publicKeyShowInstruction: {
    id: 'wallet.settings.icoPublicKeyShowInstruction',
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
  intl: intlShape.isRequired,
};

const ICOPublicKeyBox = (props: Props) => {
  const {
    publicKey,
    locale,
    onCopyWalletPublicKey,
    openDialogAction,
    intl,
  } = props;

  return (
    <>
      <BorderedBox className={styles.walletPublicKeyBox}>
        <PublicKeyField
          publicKey={publicKey || ''}
          description={intl.formatMessage(messages.publicKeyDescription)}
          locale={locale}
          onCopyPublicKey={onCopyWalletPublicKey}
          onShowQRCode={() =>
            openDialogAction({ dialog: PublicKeyQRCodeDialog })
          }
          onOpenWalletKeyDialog={() =>
            openDialogAction({ dialog: ICOPublicKeyDialog })
          }
          messages={messages}
        />
      </BorderedBox>
    </>
  );
};

export default injectIntl(ICOPublicKeyBox);

import React from 'react';
import { defineMessages, injectIntl } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSettings.scss' or its ... Remove this comment to see the full error message
import styles from './WalletSettings.scss';
import WalletPublicKeyQRCodeDialog from './WalletPublicKeyQRCodeDialog';
import PublicKeyDialog from './WalletPublicKeyDialog';
import type { Locale } from '../../../../../common/types/locales.types';
import PublicKeyField from './PublicKeyField';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

export const messages: Record<string, ReactIntlMessage> = defineMessages({
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
  publicKey: string | null | undefined;
  locale: Locale;
  onCopyWalletPublicKey: (...args: Array<any>) => any;
  openDialogAction: (...args: Array<any>) => any;
};

const WalletPublicKeyBox = (props: Props) => {
  const { publicKey, locale, onCopyWalletPublicKey, openDialogAction } = props;
  return (
    <>
      <BorderedBox className={styles.walletPublicKeyBox}>
        <PublicKeyField
          publicKey={publicKey || ''}
          locale={locale}
          onCopyPublicKey={onCopyWalletPublicKey}
          onShowQRCode={() =>
            openDialogAction({
              dialog: WalletPublicKeyQRCodeDialog,
            })
          }
          onOpenWalletKeyDialog={() =>
            openDialogAction({
              dialog: PublicKeyDialog,
            })
          }
          messages={messages}
        />
      </BorderedBox>
    </>
  );
};

export default injectIntl(WalletPublicKeyBox);

import React from 'react';
import { defineMessages, injectIntl, intlShape } from 'react-intl';
import BorderedBox from '../../widgets/BorderedBox';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSettings.scss' or its ... Remove this comment to see the full error message
import styles from './WalletSettings.scss';
import PublicKeyQRCodeDialog from './ICOPublicKeyQRCodeDialog';
import ICOPublicKeyDialog from './ICOPublicKeyDialog';
import PublicKeyField from './PublicKeyField';
import type { Locale } from '../../../../../common/types/locales.types';
import type { ReactIntlMessage } from '../../../types/i18nTypes';

export const messages: Record<string, ReactIntlMessage> = defineMessages({
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
  publicKey: string | null | undefined;
  locale: Locale;
  onCopyICOPublicKey: (...args: Array<any>) => any;
  openDialogAction: (...args: Array<any>) => any;
  intl: intlShape.isRequired;
};

const ICOPublicKeyBox = (props: Props) => {
  const {
    publicKey,
    locale,
    onCopyICOPublicKey,
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
          onCopyPublicKey={onCopyICOPublicKey}
          onShowQRCode={() =>
            openDialogAction({
              dialog: PublicKeyQRCodeDialog,
            })
          }
          onOpenWalletKeyDialog={() =>
            openDialogAction({
              dialog: ICOPublicKeyDialog,
            })
          }
          messages={messages}
        />
      </BorderedBox>
    </>
  );
};

export default injectIntl(ICOPublicKeyBox);

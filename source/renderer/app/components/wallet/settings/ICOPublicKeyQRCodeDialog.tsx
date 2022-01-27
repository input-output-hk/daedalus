// @flow
import React from 'react';
import { observer } from 'mobx-react';
import { injectIntl, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import QRCode from 'qrcode.react';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import styles from './PublicKeyQRCodeDialog.scss';
import globalMessages from '../../../i18n/global-messages';

import { messages } from './ICOPublicKeyQRCodeDialog.messages';

type Props = {
  walletName: string,
  walletPublicKey: string,
  onCopyWalletPublicKey: Function,
  onClose: Function,
  derivationPath: string,
  intl: intlShape.isRequired,
};

const ICOPublicKeyQRCodeDialog = observer((props: Props) => {
  const {
    walletName,
    walletPublicKey,
    onCopyWalletPublicKey,
    onClose,
    derivationPath,
    intl,
  } = props;

  const actions = [
    {
      label: intl.formatMessage(globalMessages.close),
      onClick: onClose,
    },
  ];

  // Get QRCode color value from active theme's CSS variable
  const qrCodeBackgroundColor = document.documentElement
    ? document.documentElement.style.getPropertyValue(
        '--theme-receive-qr-code-background-color'
      )
    : 'transparent';
  const qrCodeForegroundColor = document.documentElement
    ? document.documentElement.style.getPropertyValue(
        '--theme-receive-qr-code-foreground-color'
      )
    : '#000';

  return (
    <Dialog
      title={intl.formatMessage(messages.dialogTitle)}
      subtitle={walletName}
      actions={actions}
      closeOnOverlayClick
      onClose={onClose}
      className={styles.dialog}
      closeButton={<DialogCloseButton onClose={onClose} />}
    >
      <div className={styles.walletPublicKeyQRCode}>
        <QRCode
          value={walletPublicKey}
          bgColor={qrCodeBackgroundColor}
          fgColor={qrCodeForegroundColor}
          size={192}
        />
      </div>
      <div className={styles.addressPathsWrapper}>
        <PopOver content={intl.formatMessage(messages.derivationPathTooltip)}>
          <div className={styles.spendingPath}>{derivationPath}</div>
        </PopOver>
      </div>

      <div className={styles.walletPublicKey}>{walletPublicKey}</div>
      <CopyToClipboard text={walletPublicKey} onCopy={onCopyWalletPublicKey}>
        <span className={styles.copyPublicKey}>
          <SVGInline svg={iconCopy} className={styles.copyIcon} />
          <span className={styles.copyPublicKeyLabel}>
            {intl.formatMessage(messages.copyPublicKeyLabel)}
          </span>
        </span>
      </CopyToClipboard>
    </Dialog>
  );
});

export default injectIntl(ICOPublicKeyQRCodeDialog);

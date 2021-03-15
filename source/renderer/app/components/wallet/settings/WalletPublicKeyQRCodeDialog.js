// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import QRCode from 'qrcode.react';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import styles from './WalletPublicKeyQRCodeDialog.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
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

type Props = {
  walletName: string,
  walletPublicKey: string,
  onCopyWalletPublicKey: Function,
  onClose: Function,
};

@observer
export default class WalletPublicKeyQRCodeDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      walletName,
      walletPublicKey,
      onCopyWalletPublicKey,
      onClose,
    } = this.props;
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
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.dialog}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.walletName}>{walletName}</div>
        <div className={styles.walletPublicKeyQRCode}>
          <QRCode
            value={walletPublicKey}
            bgColor={qrCodeBackgroundColor}
            fgColor={qrCodeForegroundColor}
            size={192}
          />
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
  }
}

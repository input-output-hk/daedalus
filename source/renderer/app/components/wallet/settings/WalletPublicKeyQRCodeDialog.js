// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import QRCode from 'qrcode.react';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import styles from './WalletPublicKeyQRCodeDialog.scss';
import globalMessages from '../../../i18n/global-messages';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.settings.walletPublicKey',
    defaultMessage: '!!!Wallet Public Key',
    description: 'Title for the "Wallet Public Key QR Code" dialog.',
  },
});

type Props = {
  walletName: string,
  walletPublicKey: string,
  onClose: Function,
};

@observer
export default class WalletPublicKeyQRCodeDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { walletName, walletPublicKey, onClose } = this.props;
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
      </Dialog>
    );
  }
}

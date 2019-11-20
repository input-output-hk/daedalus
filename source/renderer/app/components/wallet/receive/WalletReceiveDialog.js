// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import copyToClipboard from 'copy-to-clipboard';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import QRCode from 'qrcode.react';
import BorderedBox from '../../widgets/BorderedBox';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletReceiveDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';

const messages = defineMessages({
  inputLabel: {
    id: 'wallet.receive.dialog.inputLabel',
    defaultMessage: '!!!PDF title',
    description: 'placeholder on the wallet "Share Address" dialog',
  },
  inputPlaceholder: {
    id: 'wallet.receive.dialog.inputPlaceholder',
    defaultMessage: '!!!Add a message for the sender',
    description: 'inputPlaceholder on the wallet "Share Address" dialog',
  },
  invalidAddressMessage: {
    id: 'wallet.receive.dialog.invalidAddressMessage',
    defaultMessage:
      '!!!This address does not match your delegation preferences. Do not use it to receive ada.',
    description: 'invalidAddressMessage on the wallet "Share Address" dialog',
  },
  downloadPDFButton: {
    id: 'wallet.receive.dialog.downloadPDFButton',
    defaultMessage: '!!!Download as PDF',
    description: 'downloadPDFButton on the wallet "Share Address" dialog',
  },
  copyAddressButton: {
    id: 'wallet.receive.dialog.copyAddressButton',
    defaultMessage: '!!!Copy address',
    description: 'copyAddressButton on the wallet "Share Address" dialog',
  },
  dialogTitle: {
    id: 'wallet.receive.dialog.dialogTitle',
    defaultMessage: '!!!Share address',
    description: 'dialogTitle on the wallet "Share Address" dialog',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  address: WalletAddress,
  onCopyAddress: Function,
  onDownloadPDF: Function,
  onClose: Function,
};

@observer
export default class WalletReceiveDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm({
    fields: {
      pdfContentTitle: {
        value: '',
        label: this.context.intl.formatMessage(messages.inputLabel),
        placeholder: this.context.intl.formatMessage(messages.inputPlaceholder),
      },
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { pdfContentTitle } = form.values();
        const { onDownloadPDF, onClose } = this.props;
        onDownloadPDF(pdfContentTitle);
        onClose();
      },
      onError: err => {
        throw new Error(err);
      },
    });
  };

  render() {
    const { address, onCopyAddress, onClose } = this.props;
    const { intl } = this.context;
    const pdfContentTitleField = this.form.$('pdfContentTitle');

    const actions = [
      {
        className: 'downloadPDFButton',
        label: intl.formatMessage(messages.downloadPDFButton),
        onClick: this.submit,
      },
      {
        className: 'copyAddressButton',
        label: intl.formatMessage(messages.copyAddressButton),
        onClick: () => {
          copyToClipboard(address.id);
          onCopyAddress(address.id);
        },
        primary: true,
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
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <BorderedBox fullHeight>
          <div className={styles.container}>
            <div className={styles.qrCode}>
              <QRCode
                value={address.id}
                bgColor={qrCodeBackgroundColor}
                fgColor={qrCodeForegroundColor}
                size={192}
              />
            </div>

            <div className={styles.address}>{address.id}</div>

            {address.isInvalid && (
              <div className={styles.invalidAddress}>
                {intl.formatMessage(messages.invalidAddressMessage)}
              </div>
            )}

            <Input
              className={styles.title}
              skin={InputSkin}
              onKeyPress={this.onChangeContentTitle}
              {...pdfContentTitleField.bind()}
            />
          </div>
        </BorderedBox>
      </Dialog>
    );
  }
}

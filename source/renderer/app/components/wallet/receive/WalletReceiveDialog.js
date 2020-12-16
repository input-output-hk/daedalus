// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import { TextArea } from 'react-polymorph/lib/components/TextArea';
import { TextAreaSkin } from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import QRCode from 'qrcode.react';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletReceiveDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';

import type { HwDeviceStatus } from '../../../domains/Wallet';

const messages = defineMessages({
  inputLabel: {
    id: 'wallet.receive.dialog.inputLabel',
    defaultMessage: '!!!PDF note',
    description: 'placeholder on the wallet "Share Address" dialog',
  },
  inputPlaceholder: {
    id: 'wallet.receive.dialog.inputPlaceholder',
    defaultMessage: '!!!Add a note to the sender',
    description: 'inputPlaceholder on the wallet "Share Address" dialog',
  },
  saveQRCodeImage: {
    id: 'wallet.receive.dialog.saveQRCodeImage',
    defaultMessage: '!!!Save QR code image',
    description: 'saveQRCodeImage on the wallet "Share Address" dialog',
  },
  downloadPDFButton: {
    id: 'wallet.receive.dialog.downloadPDFButton',
    defaultMessage: '!!!Download as PDF',
    description: 'downloadPDFButton on the wallet "Share Address" dialog',
  },
  dialogTitle: {
    id: 'wallet.receive.dialog.dialogTitle',
    defaultMessage: '!!!Share wallet address',
    description: 'dialogTitle on the wallet "Share Address" dialog',
  },
  copyAddressLabel: {
    id: 'wallet.receive.dialog.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  address: WalletAddress,
  isHardwareWallet: boolean,
  walletName: string,
  hwDeviceStatus: HwDeviceStatus,
  onCopyAddress: Function,
  onDownloadPDF: Function,
  onSaveQRCodeImage: Function,
  onClose: Function,
  onVerifyAddress: Function,
};

@observer
export default class WalletReceiveDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  UNSAFE_componentWillMount() {
    console.debug('>>> Address to Check: ', this.props.address);
    if (this.props.isHardwareWallet) {
      this.props.onVerifyAddress(this.props.address);
    }
  }

  form = new ReactToolboxMobxForm({
    fields: {
      noteInput: {
        value: '',
        label: this.context.intl.formatMessage(messages.inputLabel),
        placeholder: this.context.intl.formatMessage(messages.inputPlaceholder),
      },
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { noteInput } = form.values();
        const { onDownloadPDF } = this.props;
        onDownloadPDF(noteInput);
      },
      onError: (err) => {
        throw new Error(err);
      },
    });
  };

  handleChange = (field: { value: string }) => {
    field.value = field.value.replace(/\n/g, '');
  };

  render() {
    const { address, onCopyAddress, onSaveQRCodeImage, onClose, walletName, hwDeviceStatus, isHardwareWallet } = this.props;
    const { intl } = this.context;
    const noteInputField = this.form.$('noteInput');


    console.debug('>>> RECEIVE DIALOG: ', {
      walletName,
      hwDeviceStatus,
      isHardwareWallet,
    });

    const actions = [
      {
        label: intl.formatMessage(messages.saveQRCodeImage),
        onClick: () => onSaveQRCodeImage(),
      },
      {
        className: 'downloadPDFButton',
        label: intl.formatMessage(messages.downloadPDFButton),
        onClick: this.submit,
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
        <div className={styles.container}>
          <div className={styles.qrCode}>
            <QRCode
              value={address.id}
              bgColor={qrCodeBackgroundColor}
              fgColor={qrCodeForegroundColor}
              size={192}
            />
          </div>

          <div className={styles.hardwareWalletStatusWrapper}>
            <HardwareWalletStatus
              hwDeviceStatus={hwDeviceStatus}
              walletName={walletName}
            />
          </div>

          <div className={styles.address}>{address.id}</div>

          <CopyToClipboard
            text={address.id}
            onCopy={() => onCopyAddress(address.id)}
          >
            <span className={styles.copyAddress}>
              <SVGInline svg={iconCopy} className={styles.copyIcon} />
              <span className={styles.copyAddressLabel}>
                {intl.formatMessage(messages.copyAddressLabel)}
              </span>
            </span>
          </CopyToClipboard>

          <TextArea
            className={styles.noteInput}
            skin={TextAreaSkin}
            autoResize={false}
            rows={3}
            maxLength={201}
            {...noteInputField.bind({
              onChange: this.handleChange(noteInputField),
            })}
          />
        </div>
      </Dialog>
    );
  }
}

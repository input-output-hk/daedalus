// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
// import CopyToClipboard from 'react-copy-to-clipboard';
// import { Input } from 'react-polymorph/lib/components/Input';
// import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import QRCode from 'qrcode.react';
import BorderedBox from '../../widgets/BorderedBox';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
import styles from './WalletReceiveDialog.scss';

const messages = defineMessages({
  placeholder: {
    id: 'wallet.receive.dialog.placeholder',
    defaultMessage: '!!!Add a message for the sender',
    description: 'placeholder on the wallet "Share Address" dialog',
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
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  address: WalletAddress,
  onCopyAddress: Function,
  onClose: Function,
};

type State = {
  title: string,
};

@observer
export default class WalletReceiveDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    title: '',
  };

  render() {
    const { address, onCopyAddress, onClose } = this.props;
    const { intl } = this.context;
    // const { title } = this.state;

    const actions = [
      {
        className: 'downloadPDFButton',
        label: intl.formatMessage(messages.downloadPDFButton),
        onClick: () => {},
      },
      {
        className: 'copyAddressButton',
        label: intl.formatMessage(messages.copyAddressButton),
        onClick: () => onCopyAddress(address.id),
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
        title="Share address"
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.component}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <BorderedBox fullHeight>
          <div className={styles.container}>
            {/* <Input
              className={styles.title}
              skin={InputSkin}
              placeholder={intl.formatMessage(messages.placeholder)}
              onKeyPress={() => {}}
              value={title}
            /> */}

            <div className={styles.qrCode}>
              <QRCode
                value={address}
                bgColor={qrCodeBackgroundColor}
                fgColor={qrCodeForegroundColor}
                size={192}
              />
            </div>

            <div className={styles.address}>{address.id}</div>

            {/* <CopyToClipboard
              text={address.id}
              onCopy={() => onCopyAddress(address.id)}
            >
              <button>{intl.formatMessage(messages.placeholder)}</button>
            </CopyToClipboard> */}
          </div>
        </BorderedBox>
      </Dialog>
    );
  }
}

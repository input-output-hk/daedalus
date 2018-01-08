// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SvgInline from 'react-svg-inline';
import CopyToClipboard from 'react-copy-to-clipboard';
import QRCode from 'qrcode.react';
import BorderedBox from '../../widgets/BorderedBox';
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import styles from './WalletReceiveEtc.scss';

const messages = defineMessages({
  walletAddressLabel: {
    id: 'wallet.receive.page.etc.walletAddressLabel',
    defaultMessage: '!!!Your wallet address',
    description: 'Label for wallet address on the ETC wallet "Receive page"',
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.etc.walletReceiveInstructions',
    defaultMessage: '!!!Share this wallet address to receive payments.',
    description: 'Wallet receive payments instructions on the ETC wallet "Receive page"',
  },
});

type Props = {
  walletAddress: string,
  onCopyAddress: Function,
};

@observer
export default class WalletReceive extends Component<Props> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { walletAddress, onCopyAddress } = this.props;
    const { intl } = this.context;

    // Get QRCode color value from active theme's CSS variable
    const qrCodeBackgroundColor = document.documentElement ?
      document.documentElement.style.getPropertyValue('--theme-receive-qr-code-background-color') : 'transparent';
    const qrCodeForegroundColor = document.documentElement ?
      document.documentElement.style.getPropertyValue('--theme-receive-qr-code-foreground-color') : '#000';

    return (
      <div className={styles.component}>

        <BorderedBox>

          <div className={styles.qrCodeAndInstructions}>
            <div className={styles.qrCode}>
              <QRCode
                value={walletAddress}
                bgColor={qrCodeBackgroundColor}
                fgColor={qrCodeForegroundColor}
                size={116}
              />
            </div>

            <div className={styles.instructions}>
              <div className={styles.hash}>
                {walletAddress}
                <CopyToClipboard
                  text={walletAddress}
                  onCopy={onCopyAddress.bind(this, walletAddress)}
                >
                  <SvgInline svg={iconCopy} className={styles.copyIcon} />
                </CopyToClipboard>
              </div>

              <div className={styles.hashLabel}>
                {intl.formatMessage(messages.walletAddressLabel)}
              </div>

              <div className={styles.instructionsText}>
                {intl.formatMessage(messages.walletReceiveInstructions)}
              </div>

            </div>
          </div>

        </BorderedBox>

      </div>
    );
  }

}

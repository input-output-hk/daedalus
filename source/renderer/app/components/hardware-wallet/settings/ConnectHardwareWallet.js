// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import ledgerIcon from '../../../assets/images/hardware-wallet/ledger-cropped.inline.svg';
import ledgerXIcon from '../../../assets/images/hardware-wallet/ledger-x-cropped-outlines.inline.svg';
import trezorIcon from '../../../assets/images/hardware-wallet/trezor.inline.svg';
import unknownDeviceIcon from '../../../assets/images/hardware-wallet/trezor-ledger.inline.svg';
import styles from './ConnectHardwareWallet.scss';
import HardwareWalletStatus from '../status/HardwareWalletStatus';

const messages = defineMessages({
  hardwareWalletTitle: {
    id: 'wallet.hardware.hardwareWalletTitle',
    defaultMessage: '!!!Hardware wallet',
    description: 'Hardware wallet title.',
  },
  ledgerWalletTitle: {
    id: 'wallet.hardware.ledgerWalletTitle',
    defaultMessage: '!!!Ledger wallet',
    description: 'Ledger wallet title.',
  },
  trezorWalletTitle: {
    id: 'wallet.hardware.trezorWalletTitle',
    defaultMessage: '!!!Trezor wallet',
    description: 'Trezor wallet title.',
  },
  hardwareWalletInstructions: {
    id: 'wallet.hardware.hardwareWalletInstructions',
    defaultMessage: '!!!Follow instructions to access your wallet',
    description: 'Follow instructions label',
  },
});

type Props = {
  onOpenExternalLink: Function,
  isLedger: boolean,
  isTrezor: boolean,
  isDeviceConnected: boolean | null,
  fetchingDevice: boolean,
  isExportingExtendedPublicKey: boolean | null,
  isExportingPublicKeyAborted: boolean,
  isExtendedPublicKeyExported: boolean,
  isCardanoAppLaunched: boolean,
};

@observer
export default class ConnectHardwareWallet extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const {
      onOpenExternalLink,
      isLedger,
      isTrezor,
      isDeviceConnected,
      fetchingDevice,
      isExportingExtendedPublicKey,
      isExportingPublicKeyAborted,
      isExtendedPublicKeyExported,
      isCardanoAppLaunched,
    } = this.props;

    let hardwareTitle = intl.formatMessage(messages.hardwareWalletTitle);
    if (isTrezor) {
      hardwareTitle = intl.formatMessage(messages.trezorWalletTitle);
    } else if (isLedger) {
      hardwareTitle = intl.formatMessage(messages.ledgerWalletTitle);
    }

    return (
      <div className={styles.component}>
        <div className={styles.hardwareWalletContainer}>
          <div className={styles.hardwareWalletWrapper}>
            {!isTrezor && !isLedger && (
              <div className={styles.hardwareWalletUnknown}>
                <SVGInline
                  svg={unknownDeviceIcon}
                  className={styles.unknownDeviceIcon}
                />
              </div>
            )}
            {isLedger && (
              <div className={styles.hardwareWalletLedger}>
                <SVGInline svg={ledgerXIcon} className={styles.ledgerXIcon} />
                <SVGInline svg={ledgerIcon} className={styles.ledgerIcon} />
              </div>
            )}
            {isTrezor && (
              <div className={styles.hardwareWalletTrezor}>
                <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
              </div>
            )}
            <h2 className={styles.hardwareWalletTitle}>{hardwareTitle}</h2>
            <p className={styles.hardwareWalletMessage}>
              {intl.formatMessage(messages.hardwareWalletInstructions)}
            </p>
            <div className={styles.hardwareWalletStepsWrapper}>
              <HardwareWalletStatus
                onOpenExternalLink={onOpenExternalLink}
                isDeviceConnected={isDeviceConnected}
                fetchingDevice={fetchingDevice}
                isExportingExtendedPublicKey={isExportingExtendedPublicKey}
                isExportingPublicKeyAborted={isExportingPublicKeyAborted}
                isExtendedPublicKeyExported={isExtendedPublicKeyExported}
                isTrezor={isTrezor}
                isLedger={isLedger}
                isCardanoAppLaunched={isCardanoAppLaunched}
              />
            </div>
          </div>
        </div>
      </div>
    );
  }
}

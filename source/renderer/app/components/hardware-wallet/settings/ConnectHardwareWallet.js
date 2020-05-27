// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import ledgerIcon from '../../../assets/images/hardware-wallet/ledger-cropped.inline.svg';
import ledgerXIcon from '../../../assets/images/hardware-wallet/ledger-x-cropped-outlines.inline.svg';
import trezorIcon from '../../../assets/images/hardware-wallet/trezor-ledger.inline.svg';
import exportIcon from '../../../assets/images/hardware-wallet/export.inline.svg';
import checkIcon from '../../../assets/images/hardware-wallet/check.inline.svg';
import clearIcon from '../../../assets/images/hardware-wallet/close-cross-red.inline.svg';
import ledgerSmallIcon from '../../../assets/images/hardware-wallet/ledger-bold-ic.inline.svg';
import styles from './ConnectHardwareWallet.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';

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
  hardwareWalletInstructions: {
    id: 'wallet.hardware.hardwareWalletInstructions',
    defaultMessage: '!!!Follow instructions to access your wallet',
    description: 'Follow instructions label',
  },
  hardwareWalletLedgerBegin: {
    id: 'wallet.hardware.hardwareWalletLedgerBegin',
    defaultMessage:
      '!!!To begin, connect and unlock your <span>Ledger Device</span>',
    description: 'Connect device label',
  },
  hardwareWalletBegin: {
    id: 'wallet.hardware.hardwareWalletBegin',
    defaultMessage:
      '!!!To begin, connect and unlock your <span>Hardware wallet Device</span>',
    description: 'Connect device label',
  },
  hardwareWalletExport: {
    id: 'wallet.hardware.hardwareWalletExport',
    defaultMessage: '!!!Export <span>public key</span> on your device',
    description: 'Export wallet label',
  },
  linkUrl: {
    id: 'wallet.select.import.dialog.linkUrl',
    defaultMessage: '!!!https://daedaluswallet.io/',
    description: 'External link URL on the hardware wallet connect screen',
  },
});

type Props = {
  onOpenExternalLink: Function,
  isLedger: boolean,
  isTrezor: boolean,
  isDeviceConnected: boolean | null,
  fetchingDevice: boolean,
  exportingExtendedPublicKey: boolean | null,
  isExportingPublicKeyAborted: boolean,
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
      exportingExtendedPublicKey,
      isExportingPublicKeyAborted,
    } = this.props;

    const hardwareTitle = isTrezor
      ? intl.formatMessage(messages.hardwareWalletTitle)
      : intl.formatMessage(messages.ledgerWalletTitle);

    const hardwareConnectLabel = isTrezor
      ? messages.hardwareWalletBegin
      : messages.hardwareWalletLedgerBegin;

    const firstStepClasses = classnames([
      styles.hardwareWalletStep,
      fetchingDevice ? styles.isActiveFetchingDevice : null,
      isDeviceConnected === null ? styles.isErrorDevice : null,
    ]);

    const secondStepClasses = classnames([
      styles.hardwareWalletStep,
      exportingExtendedPublicKey ? styles.isActiveExport : null,
      isExportingPublicKeyAborted ? styles.isErrorExport : null,
    ]);


    console.debug('>>> IS DEVICE CONNECTED: ', isDeviceConnected);

    return (
      <div className={styles.component}>
        <div className={styles.hardwareWalletContainer}>
          <div className={styles.hardwareWalletWrapper}>
            {isTrezor && (
              <div className={styles.hardwareWalletTrezor}>
                <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
              </div>
            )}
            {isLedger && (
              <div className={styles.hardwareWalletLedger}>
                <SVGInline svg={ledgerXIcon} className={styles.ledgerXIcon} />
                <SVGInline svg={ledgerIcon} className={styles.ledgerIcon} />
              </div>
            )}
            <h2 className={styles.hardwareWalletTitle}>{hardwareTitle}</h2>
            <p className={styles.hardwareWalletMessage}>
              {intl.formatMessage(messages.hardwareWalletInstructions)}
            </p>
            <div className={styles.hardwareWalletStepsWrapper}>
              <div className={firstStepClasses}>
                <div className={styles.hardwareWalletInnerStep}>
                  <SVGInline
                    svg={ledgerSmallIcon}
                    className={styles.ledgerSmallIcon}
                  />
                  <FormattedHTMLMessage {...hardwareConnectLabel} />
                </div>
                {fetchingDevice && <LoadingSpinner />}
                {isDeviceConnected && (
                  <SVGInline svg={checkIcon} className={styles.checkIcon} />
                )}
                {!fetchingDevice && isDeviceConnected === null && (
                  <SVGInline svg={clearIcon} className={styles.clearIcon} />
                )}
              </div>
              <div className={secondStepClasses}>
                <div className={styles.hardwareWalletInnerStep}>
                  <SVGInline
                    svg={exportIcon}
                    className={styles.exportIcon}
                    onClick={() =>
                      onOpenExternalLink(intl.formatMessage(messages.linkUrl))
                    }
                  />
                  <FormattedHTMLMessage {...messages.hardwareWalletExport} />
                </div>
                {exportingExtendedPublicKey && <LoadingSpinner />}
                {!isExportingPublicKeyAborted &&
                  exportingExtendedPublicKey !== true &&
                  exportingExtendedPublicKey !== null && (
                    <SVGInline svg={checkIcon} className={styles.checkIcon} />
                  )}
                {!exportingExtendedPublicKey && isExportingPublicKeyAborted && (
                  <SVGInline svg={clearIcon} className={styles.clearIcon} />
                )}
              </div>
            </div>
          </div>
        </div>
      </div>
    );
  }
}

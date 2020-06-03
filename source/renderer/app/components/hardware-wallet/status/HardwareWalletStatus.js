// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import exportIcon from '../../../assets/images/hardware-wallet/export.inline.svg';
import checkIcon from '../../../assets/images/hardware-wallet/check.inline.svg';
import clearIcon from '../../../assets/images/hardware-wallet/close-cross-red.inline.svg';
import ledgerSmallIcon from '../../../assets/images/hardware-wallet/ledger-bold-ic.inline.svg';
import styles from './HardwareWalletStatus.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { WalletStatus } from '../../../types/walletRestoreTypes';

const messages = defineMessages({
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
  hardwareWalletTrezorBegin: {
    id: 'wallet.hardware.hardwareWalletTrezorBegin',
    defaultMessage:
      '!!!To begin, connect and unlock your <span>Trezor Device</span>',
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
  hardwareWalletExportRejected: {
    id: 'wallet.hardware.hardwareWalletExportRejected',
    defaultMessage: '!!!Export rejected',
    description: 'Export wallet rejected label',
  },
  linkUrl: {
    id: 'wallet.select.import.dialog.linkUrl',
    defaultMessage: '!!!https://daedaluswallet.io/',
    description: 'External link URL on the hardware wallet connect screen',
  },
  openCardanoAppLabel: {
    id: 'wallet.hardware.openCardanoAppLabel',
    defaultMessage: '!!!Open <span>Cardano app</span>',
    description: 'Connected but Cardano app not launched',
  },
  deviceConnectedLabel: {
    id: 'wallet.hardware.deviceConnectedLabel',
    defaultMessage: '!!!{deviceType} device',
    description: 'Connected / Accepted device label',
  },
});

type Props = {
  onOpenExternalLink: Function,
  isLedger: boolean,
  isTrezor: boolean,
  isCardanoAppLaunched: boolean,
  walletStatus: WalletStatus,
};

@observer
export default class HardwareWalletStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const {
      onOpenExternalLink,
      isLedger,
      isTrezor,
      isCardanoAppLaunched,
      walletStatus,
    } = this.props;

    let hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletBegin);
    if (isTrezor) {
      hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletTrezorBegin);
    } else if (isLedger) {
      hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletLedgerBegin);
    }

    const walletStepClasses = classnames([
      styles.hardwareWalletStep,
      !walletStatus ? styles.isError : null,
      walletStatus === 'connect' ? styles.isActiveFetching : null,
      walletStatus === 'export' ? styles.isActiveExport : null,
      walletStatus === 'verifyTx' ? styles.isVerifyTx : null,
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.hardwareWalletStepsWrapper}>
          <div className={walletStepClasses}>
            <div className={styles.hardwareWalletInnerStep}>
              <SVGInline
                svg={ledgerSmallIcon}
                className={styles.ledgerSmallIcon}
              />
              {(!isDeviceConnected && !isCardanoAppLaunched && fetchingDevice) &&
              <FormattedHTMLMessage {...hardwareConnectLabel} />
              }
              {(isDeviceConnected && !isCardanoAppLaunched && fetchingDevice) &&
              <FormattedHTMLMessage {...messages.openCardanoAppLabel} />
              }
              {(isDeviceConnected && isCardanoAppLaunched && !fetchingDevice) &&
              <FormattedHTMLMessage
                {...messages.deviceConnectedLabel}
                values={{
                  deviceType: isLedger ? 'Ledger' : 'Trezor',
                }}
              />
              }
            </div>
            {fetchingDevice && <LoadingSpinner/>}
            {(isDeviceConnected && isCardanoAppLaunched && !fetchingDevice) && (
              <SVGInline svg={checkIcon} className={styles.checkIcon}/>
            )}
            {!fetchingDevice && !isDeviceConnected && (
              <SVGInline svg={clearIcon} className={styles.clearIcon}/>
            )}
            <SVGInline
              svg={exportIcon}
              className={styles.exportIcon}
              onClick={() =>
                onOpenExternalLink(intl.formatMessage(messages.linkUrl))
              }
            />
            {!isExportingPublicKeyAborted && (
              <FormattedHTMLMessage {...messages.hardwareWalletExport} />
            )}
            {isExportingPublicKeyAborted && (
              <FormattedHTMLMessage
                {...messages.hardwareWalletExportRejected}
              />
            )}
            {isExportingExtendedPublicKey && <LoadingSpinner/>}
            {isExtendedPublicKeyExported && (
              <SVGInline svg={checkIcon} className={styles.checkIcon}/>
            )}
            {isExportingPublicKeyAborted && (
              <SVGInline svg={clearIcon} className={styles.clearIcon}/>
            )}
          </div>
        </div>
      </div>
    );
  }
}

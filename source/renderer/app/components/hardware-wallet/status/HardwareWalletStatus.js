// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import checkIcon from '../../../assets/images/hardware-wallet/check.inline.svg';
import clearIcon from '../../../assets/images/hardware-wallet/close-cross-red.inline.svg';
import styles from './HardwareWalletStatus.scss';
import LoadingSpinner from '../../widgets/LoadingSpinner';

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
  isDeviceConnected: boolean,
  fetchingDevice: boolean,
  isExportingExtendedPublicKey: boolean,
  isExportingPublicKeyAborted: boolean,
  isExtendedPublicKeyExported: boolean,
  isLedger: boolean,
  isTrezor: boolean,
  isCardanoAppLaunched: boolean,
};

@observer
export default class HardwareWalletStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;

    const {
      isDeviceConnected,
      fetchingDevice,
      isExportingExtendedPublicKey,
      isExportingPublicKeyAborted,
      isExtendedPublicKeyExported,
      isLedger,
      isTrezor,
      isCardanoAppLaunched,
    } = this.props;

    let hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletBegin);
    if (isTrezor) {
      hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletTrezorBegin);
    } else if (isLedger) {
      hardwareConnectLabel = intl.formatMessage(messages.hardwareWalletLedgerBegin);
    }

    const walletStepClasses = classnames([
      styles.hardwareWalletStep,
      fetchingDevice ? styles.isActiveFetching : null,
      isExportingExtendedPublicKey ? styles.isActiveExport : null,
      isExportingPublicKeyAborted ? styles.isErrorExport : null,
    ]);

    return (
      <div className={walletStepClasses}>
        <div className={styles.hardwareWalletInnerStep}>
          {!isCardanoAppLaunched && <FormattedHTMLMessage {...messages.openCardanoAppLabel} />}
          {<FormattedHTMLMessage {...hardwareConnectLabel} />}
          {<FormattedHTMLMessage
            {...messages.deviceConnectedLabel}
            values={{
              deviceType: isLedger ? 'Ledger' : 'Trezor',
            }}
          />}
          {<FormattedHTMLMessage {...messages.hardwareWalletExport} />}
          {<FormattedHTMLMessage
            {...messages.hardwareWalletExportRejected}
          />}
        </div>
        {(!isCardanoAppLaunched || (isCardanoAppLaunched && (fetchingDevice || isExportingExtendedPublicKey))) && <LoadingSpinner/>}
        {(isCardanoAppLaunched && (isDeviceConnected || isExtendedPublicKeyExported)) && <SVGInline svg={checkIcon} className={styles.checkIcon}/>}
        {(isCardanoAppLaunched && (fetchingDevice === null || isExportingPublicKeyAborted)) && <SVGInline svg={clearIcon} className={styles.clearIcon}/>}
      </div>
    );
  }
}

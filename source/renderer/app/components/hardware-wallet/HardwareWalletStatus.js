// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import checkIcon from '../../assets/images/hardware-wallet/check.inline.svg';
import clearIcon from '../../assets/images/hardware-wallet/close-cross-red.inline.svg';
import LoadingSpinner from '../widgets/LoadingSpinner';
import { HwDeviceStatuses } from '../../domains/Wallet';
import type { HwDeviceStatus } from '../../domains/Wallet';
import styles from './HardwareWalletStatus.scss';

const messages = defineMessages({
  connecting: {
    id: 'wallet.hardware.deviceStatus.connecting',
    defaultMessage: '!!!Connect your device and enter your PIN to unlock it',
    description:
      '"Connect your device and enter your PIN to unlock it" device state',
  },
  launching_cardano_app: {
    id: 'wallet.hardware.deviceStatus.launching_cardano_app',
    defaultMessage: '!!!Launch the Cardano application on your device',
    description: '"Launch the Cardano application on your device" device state',
  },
  exporting_public_key: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key',
    defaultMessage: '!!!Confirm exporting your public key on your device',
    description:
      '"Confirm exporting your public key on your device" device state',
  },
  exporting_public_key_failed: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key_failed',
    defaultMessage: '!!!Exporting public key failed',
    description: '"Exporting public key failed" device state',
  },
  exportingPublicKeyError: {
    id: 'wallet.hardware.deviceStatus.exportingPublicKeyError',
    defaultMessage:
      '!!!Disconnect and reconnect your device to start the process again',
    description:
      '"Disconnect and reconnect your device to start the process again" device state',
  },
  ready: {
    id: 'wallet.hardware.deviceStatus.ready',
    defaultMessage: '!!!Device ready and waiting for commands',
    description: '"Device ready and waiting for commands" device state',
  },
  verifying_transaction: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction',
    defaultMessage:
      '!!!Verify the transaction on your device in order to sign it',
    description:
      '"Verify the transaction on your device in order to sign it" device state',
  },
  verifying_transaction_failed: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_failed',
    defaultMessage: '!!!Transaction verification and signing failed',
    description: '"Transaction verification and signing failed" device state',
  },
  verifying_transaction_succeeded: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_succeeded',
    defaultMessage: '!!!Transaction verified and signed',
    description: '"Transaction verified and signed" device state',
  },
});

type Props = {
  hwDeviceStatus: HwDeviceStatus,
};

@observer
export default class HardwareWalletStatus extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { hwDeviceStatus } = this.props;

    const isLoading =
      hwDeviceStatus === HwDeviceStatuses.CONNECTING ||
      hwDeviceStatus === HwDeviceStatuses.LAUNCHING_CARDANO_APP ||
      hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION;

    const isReady =
      hwDeviceStatus === HwDeviceStatuses.READY ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;

    const hasErrored =
      hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED;

    const componentClasses = classnames([
      styles.component,
      isReady ? styles.isReady : null,
      hasErrored ? styles.isError : null,
    ]);

    return (
      <>
        <div className={componentClasses}>
          <div className={styles.message}>
            {intl.formatMessage(messages[hwDeviceStatus])}
          </div>
          {isLoading && <LoadingSpinner className='hardwareWalletProcessProgress' />}
          {isReady && (
            <SVGInline svg={checkIcon} className={styles.checkIcon} />
          )}
          {hasErrored && (
            <SVGInline svg={clearIcon} className={styles.clearIcon} />
          )}
        </div>
        {hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED && (
          <div className={styles.errorText}>
            {intl.formatMessage(messages.exportingPublicKeyError)}
          </div>
        )}
      </>
    );
  }
}
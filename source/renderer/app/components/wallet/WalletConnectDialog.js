// @flow
// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import ledgerIcon from '../../assets/images/hardware-wallet/ledger-cropped.inline.svg';
import ledgerXIcon from '../../assets/images/hardware-wallet/ledger-x-cropped-outlines.inline.svg';
import trezorIcon from '../../assets/images/hardware-wallet/trezor.inline.svg';
import unknownDeviceIcon from '../../assets/images/hardware-wallet/trezor-ledger.inline.svg';
import DialogCloseButton from '../widgets/DialogCloseButton';
import LocalizableError from '../../i18n/LocalizableError';
import Dialog from '../widgets/Dialog';
import styles from './WalletConnectDialog.scss';
import LoadingSpinner from '../widgets/LoadingSpinner';
import HardwareWalletStatus from '../hardware-wallet/HardwareWalletStatus';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../common/types/hardware-wallets.types';

import type { TransportDevice } from '../../../../common/types/hardware-wallets.types';
import type { HwDeviceStatus } from '../../domains/Wallet';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.connect.dialog.title',
    defaultMessage: '!!!Connect hardware wallet',
    description: 'Title "Connect hardware wallet" in the connect wallet dialog.',
  },
  closeButton: {
    id: 'wallet.connect.dialog.button.close',
    defaultMessage: '!!!Cancel',
    description:
      'Label for the "Cancel" button in the connect wallet dialog',
  },
  hardwareWalletInstructions: {
    id: 'wallet.connect.hardwareWalletInstructions',
    defaultMessage: '!!!Follow instructions to access your wallet',
    description: 'Follow instructions label',
  },
});

type Props = {
  onClose: Function,
  isSubmitting: boolean,
  hwDeviceStatus: HwDeviceStatus,
  transportDevice: TransportDevice,
  error: ?LocalizableError,
};


@observer
export default class WalletConnectDialog extends Component<State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onClose, isSubmitting, hwDeviceStatus, transportDevice, error } = this.props;

    const isLedger = transportDevice && transportDevice.deviceType === DeviceTypes.LEDGER;
    const isTrezor = transportDevice && transportDevice.deviceType === DeviceTypes.TREZOR;
    const dialogClasses = classnames([styles.component, 'WalletConnectDialog']);

    const buttonLabel = !isSubmitting ? (
      this.context.intl.formatMessage(messages.closeButton)
    ) : (
      <LoadingSpinner />
    );

    const actions = [
      {
        disabled: isSubmitting,
        label: buttonLabel,
        primary: false,
        onClick: onClose,
      },
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick={false}
        onClose={!isSubmitting ? onClose : () => {}}
        closeButton={<DialogCloseButton />}
      >

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
                {transportDevice.deviceModel === DeviceModels.LEDGER_NANO_X && (
                  <SVGInline svg={ledgerXIcon} className={styles.ledgerXIcon} />
                )}
                {transportDevice.deviceModel === DeviceModels.LEDGER_NANO_S && (
                  <SVGInline svg={ledgerIcon} className={styles.ledgerIcon} />
                )}
              </div>
            )}
            {isTrezor && (
              <div className={styles.hardwareWalletTrezor}>
                <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
              </div>
            )}
            {error ? (
              <p className={styles.error}>{intl.formatMessage(error)}</p>
            ) : (
              <div>
                <p className={styles.hardwareWalletMessage}>
                  {intl.formatMessage(messages.hardwareWalletInstructions)}
                </p>
                <div className={styles.hardwareWalletStatusWrapper}>
                  <HardwareWalletStatus hwDeviceStatus={hwDeviceStatus} />
                </div>
              </div>
            )}
          </div>

      </Dialog>
    );
  }
}

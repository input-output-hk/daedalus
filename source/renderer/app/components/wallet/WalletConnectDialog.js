// @flow
// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import { get } from 'lodash';
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
  isLedgerEnabled,
  isTrezorEnabled,
} from '../../config/hardwareWalletsConfig';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../../common/types/hardware-wallets.types';

import type { TransportDevice } from '../../../../common/types/hardware-wallets.types';
import type { HwDeviceStatus } from '../../domains/Wallet';

const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.connect.dialog.title',
    defaultMessage: '!!!Pair a hardware wallet device',
    description:
      'Title "Connect a hardware wallet device" in the connect wallet dialog.',
  },
  cancelButton: {
    id: 'wallet.connect.dialog.button.cancel',
    defaultMessage: '!!!Cancel',
    description: 'Label for the "Cancel" button in the connect wallet dialog',
  },
  instructions: {
    id: 'wallet.connect.dialog.instructions',
    defaultMessage:
      '!!!<p><b>Daedalus currently supports only Trezor Model T hardware wallet devices.</b></p><p>If you are <b>pairing your device with Daedalus for the first time</b>, please follow the instructions below.</p><p>If you have <b>already paired your device with Daedalus</b>, you don’t need to repeat this step. Just connect your device when you need to confirm a transaction.</p>',
    description: 'Follow instructions label',
  },
});

type Props = {
  onClose: Function,
  isSubmitting: boolean,
  hwDeviceStatus: HwDeviceStatus,
  transportDevice: ?TransportDevice,
  error: ?LocalizableError,
  onExternalLinkClick: Function,
};

@observer
export default class WalletConnectDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      isSubmitting,
      hwDeviceStatus,
      transportDevice,
      onExternalLinkClick,
      error,
    } = this.props;

    const deviceType = get(transportDevice, 'deviceType');
    const deviceModel = get(transportDevice, 'deviceModel');

    const isLedger = deviceType === DeviceTypes.LEDGER;
    const isTrezor = deviceType === DeviceTypes.TREZOR;
    const dialogClasses = classnames([styles.component, 'WalletConnectDialog']);

    const buttonLabel = !isSubmitting ? (
      this.context.intl.formatMessage(messages.cancelButton)
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

    const renderUnknownDevice = () => {
      let unknownDeviceElement;
      if (isTrezorEnabled && !isLedgerEnabled) {
        unknownDeviceElement = (
          <div className={styles.hardwareWalletTrezor}>
            <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
          </div>
        );
      } else if (isLedgerEnabled && !isTrezorEnabled) {
        unknownDeviceElement = (
          <div className={styles.hardwareWalletLedger}>
            <SVGInline svg={ledgerIcon} className={styles.ledgerIcon} />
          </div>
        );
      } else {
        unknownDeviceElement = (
          <div className={styles.hardwareWalletUnknown}>
            <SVGInline
              svg={unknownDeviceIcon}
              className={styles.unknownDeviceIcon}
            />
          </div>
        );
      }
      return unknownDeviceElement;
    };

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
          {(!transportDevice || (!isTrezor && !isLedger)) &&
            renderUnknownDevice()}
          {isLedger && isLedgerEnabled && (
            <div className={styles.hardwareWalletLedger}>
              {deviceModel === DeviceModels.LEDGER_NANO_X && (
                <SVGInline svg={ledgerXIcon} className={styles.ledgerXIcon} />
              )}
              {deviceModel === DeviceModels.LEDGER_NANO_S && (
                <SVGInline svg={ledgerIcon} className={styles.ledgerIcon} />
              )}
            </div>
          )}
          {isTrezor && isTrezorEnabled && (
            <div className={styles.hardwareWalletTrezor}>
              <SVGInline svg={trezorIcon} className={styles.trezorIcon} />
            </div>
          )}
          {error ? (
            <p className={styles.error}>{intl.formatMessage(error)}</p>
          ) : (
            <div>
              <p className={styles.hardwareWalletMessage}>
                <FormattedHTMLMessage {...messages.instructions} />
              </p>
              <div className={styles.hardwareWalletStatusWrapper}>
                <HardwareWalletStatus
                  hwDeviceStatus={hwDeviceStatus}
                  onExternalLinkClick={onExternalLinkClick}
                  isTransactionStatus={false}
                />
              </div>
            </div>
          )}
        </div>
      </Dialog>
    );
  }
}

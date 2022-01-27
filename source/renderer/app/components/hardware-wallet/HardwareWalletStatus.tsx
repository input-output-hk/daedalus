import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import checkIcon from '../../assets/images/hardware-wallet/check.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import clearIcon from '../../assets/images/hardware-wallet/close-cross-red.inline.svg';
import LoadingSpinner from '../widgets/LoadingSpinner';
import { HwDeviceStatuses } from '../../domains/Wallet';
import type { HwDeviceStatus } from '../../domains/Wallet';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './HardwareWalletStatus.scss' o... Remove this comment to see the full error message
import styles from './HardwareWalletStatus.scss';

const messages = defineMessages({
  connecting: {
    id: 'wallet.hardware.deviceStatus.connecting',
    defaultMessage: '!!!Connect your device and enter your PIN to unlock it',
    description:
      '"Connect your device and enter your PIN to unlock it" device state',
  },
  connecting_failed: {
    id: 'wallet.hardware.deviceStatus.connecting.failed',
    defaultMessage:
      '!!!Disconnect and reconnect your device to restart the process.',
    description: '"Connect failed" device state',
  },
  connecting_known: {
    id: 'wallet.hardware.deviceStatus.connecting.known',
    defaultMessage: '!!!Connect the "{walletName}" device',
    description: '"Connect the IOHK Trezor 1 device" device state',
  },
  launching_cardano_app: {
    id: 'wallet.hardware.deviceStatus.launching_cardano_app',
    defaultMessage: '!!!Launch Cardano application on your device',
    description: '"Launch Cardano application on your device" device state',
  },
  exporting_public_key: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key',
    defaultMessage: '!!!Export the public key on your device',
    description:
      '"Confirm exporting your public key on your device" device state',
  },
  exporting_public_key_failed: {
    id: 'wallet.hardware.deviceStatus.exporting_public_key_failed',
    defaultMessage: '!!!Exporting the public key failed',
    description: '"Exporting public key failed" device state',
  },
  exportingPublicKeyError: {
    id: 'wallet.hardware.deviceStatus.exportingPublicKeyError',
    defaultMessage:
      '!!!Disconnect and reconnect your device to restart the process.',
    description:
      '"Disconnect and reconnect your device to start the process again" device state',
  },
  enterPassphrase: {
    id: 'wallet.hardware.deviceStatus.enterPassphrase',
    defaultMessage: '!!!Enter passphrase if needed',
    description: '"Enter passphrase if needed" device sub-state',
  },
  ready: {
    id: 'wallet.hardware.deviceStatus.ready',
    defaultMessage: '!!!Device ready',
    description: '"Device ready" device state',
  },
  verifying_transaction: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction',
    defaultMessage:
      '!!!Confirm the transaction using the "{walletName}" device',
    description:
      '"Confirm the transaction using the IOHK Trezor 1 device" device state',
  },
  verifying_transaction_failed: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_failed',
    defaultMessage: '!!!Transaction verification and signing failed',
    description: '"Transaction verification and signing failed" device state',
  },
  verifying_transaction_succeeded: {
    id: 'wallet.hardware.deviceStatus.verifying_transaction_succeeded',
    defaultMessage: '!!!Transaction confirmed',
    description: '"Transaction verified and signed" device state',
  },
  trezor_bridge_failure: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure',
    defaultMessage: '!!!Trezor Bridge not installed!',
    description:
      '"Trezor Bridge not installed! {instructionsLink}" device state',
  },
  trezor_bridge_failure_link_label: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure.link.label',
    defaultMessage: '!!!Installation instructions',
    description: 'Trezor Bridge installation instructions link label',
  },
  trezor_bridge_failure_link_url: {
    id: 'wallet.hardware.deviceStatus.trezor_bridge_failure.link.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011451693',
    description: 'URL for the "Trezor Bridge" update',
  },
  wrong_firmware: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware',
    defaultMessage: '!!!Unsupported firmware! {instructionsLink}',
    description: '"Unsupported firmware!" device state',
  },
  wrong_firmware_link_label: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware.link.label',
    defaultMessage: '!!!Firmware update instructions',
    description: 'Firmware update installation instructions link label',
  },
  wrong_firmware_link_url: {
    id: 'wallet.hardware.deviceStatus.wrong_firmware.link.url',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011451693',
    description: 'URL for the "Firmware Update"',
  },
  unsupported_device: {
    id: 'wallet.hardware.deviceStatus.unsupported_device',
    defaultMessage: '!!!The device is not supported!',
    description: '"The device is not supported!" device state',
  },
  wrong_cardano_app_version: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version',
    defaultMessage: '!!!Outdated Ledger software!! {instructionsLink}',
    description: '"Unsupported firmware!" device state',
  },
  wrong_cardano_app_version_link_label: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version.link.label',
    defaultMessage: '!!!Software update instructions',
    description: 'Firmware update installation instructions link label',
  },
  wrong_cardano_app_version_link_url: {
    id: 'wallet.hardware.deviceStatus.wrong_cardano_app_version.link.url',
    defaultMessage:
      '!!!https://support.ledger.com/hc/en-us/articles/360020095874-Cardano-ADA-',
    description: 'URL for the "Firmware Update"',
  },
  verifying_address: {
    id: 'wallet.hardware.deviceStatus.verifying_address',
    defaultMessage: '!!!Verify address on your "{walletName}" device',
    description: '"Verify receiving address on your Hardware Wallet device',
  },
  verifying_address_confirmation: {
    id: 'wallet.hardware.deviceStatus.verifying_address_confirmation',
    defaultMessage: '!!!Please answer the question below',
    description: '"Confirm receiving address on your Hardware Wallet device',
  },
  verifying_address_failed: {
    id: 'wallet.hardware.deviceStatus.verifying_address_failed',
    defaultMessage: '!!!Address verification failed',
    description: '"Address verification failed" device state',
  },
  verifying_address_aborted: {
    id: 'wallet.hardware.deviceStatus.verifying_address_aborted',
    defaultMessage: '!!!Verification was aborted by the user',
    description: '"Address verification aborted" device state',
  },
  verifying_address_succeeded: {
    id: 'wallet.hardware.deviceStatus.verifying_address_succeeded',
    defaultMessage: '!!!Address verified',
    description: '"Address verified" device state',
  },
});
type Props = {
  hwDeviceStatus: HwDeviceStatus;
  onExternalLinkClick?: (...args: Array<any>) => any;
  walletName?: string;
  isTrezor: boolean;
};
type State = {
  hwDeviceStatus: HwDeviceStatus;
};

@observer
class HardwareWalletStatus extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    hwDeviceStatus: this.props.hwDeviceStatus,
  };

  // eslint-disable-next-line
  UNSAFE_componentWillReceiveProps(nextProps: Props) {
    if (nextProps.hwDeviceStatus !== this.props.hwDeviceStatus) {
      // Set with delay
      if (
        nextProps.hwDeviceStatus === HwDeviceStatuses.CONNECTING &&
        this.props.hwDeviceStatus === HwDeviceStatuses.LAUNCHING_CARDANO_APP
      ) {
        setTimeout(() => {
          // Status remains unchanged in 1.5s - set as new status
          if (nextProps.hwDeviceStatus === this.props.hwDeviceStatus) {
            this.setState({
              hwDeviceStatus: nextProps.hwDeviceStatus,
            });
          }
        }, 4000);
      } else {
        this.setState({
          hwDeviceStatus: nextProps.hwDeviceStatus,
        });
      }
    }
  }

  render() {
    const { intl } = this.context;
    const { onExternalLinkClick, walletName, isTrezor } = this.props;
    const { hwDeviceStatus } = this.state;
    const isLoading =
      hwDeviceStatus === HwDeviceStatuses.CONNECTING ||
      hwDeviceStatus === HwDeviceStatuses.LAUNCHING_CARDANO_APP ||
      hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION;
    const isReady =
      hwDeviceStatus === HwDeviceStatuses.READY ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS_SUCCEEDED;
    const hasErrored =
      hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY_FAILED ||
      hwDeviceStatus === HwDeviceStatuses.CONNECTING_FAILED ||
      hwDeviceStatus === HwDeviceStatuses.TREZOR_BRIDGE_FAILURE ||
      hwDeviceStatus === HwDeviceStatuses.WRONG_FIRMWARE ||
      hwDeviceStatus === HwDeviceStatuses.WRONG_CARDANO_APP_VERSION ||
      hwDeviceStatus === HwDeviceStatuses.UNSUPPORTED_DEVICE ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION_FAILED ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS_FAILED ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS_ABORTED;
    const hasPassphraseLabel =
      hwDeviceStatus === HwDeviceStatuses.EXPORTING_PUBLIC_KEY ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION ||
      hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS;
    const componentClasses = classnames([
      styles.component,
      isReady ? styles.isReady : null,
      hasErrored ? styles.isError : null,
    ]);
    const hasInstructionsLink =
      hwDeviceStatus === HwDeviceStatuses.TREZOR_BRIDGE_FAILURE ||
      hwDeviceStatus === HwDeviceStatuses.WRONG_CARDANO_APP_VERSION ||
      hwDeviceStatus === HwDeviceStatuses.WRONG_FIRMWARE;
    let instructionsLink;
    let label;

    if (hasInstructionsLink && onExternalLinkClick) {
      // @TODO - add Ledger firmware update support article links
      instructionsLink = (
        <Link
          className={styles.externalLink}
          onClick={(event) =>
            onExternalLinkClick(
              intl.formatMessage(messages[`${hwDeviceStatus}_link_url`]),
              event
            )
          }
          label={intl.formatMessage(messages[`${hwDeviceStatus}_link_label`])}
          skin={LinkSkin}
        />
      );
    } else if (
      walletName &&
      (hwDeviceStatus === HwDeviceStatuses.CONNECTING ||
        hwDeviceStatus === HwDeviceStatuses.VERIFYING_TRANSACTION ||
        hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS ||
        hwDeviceStatus === HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION)
    ) {
      const message =
        hwDeviceStatus === HwDeviceStatuses.CONNECTING
          ? `${hwDeviceStatus}_known`
          : hwDeviceStatus;
      label = (
        <FormattedMessage
          {...messages[message]}
          values={{
            walletName,
          }}
        />
      );
    } else {
      label = intl.formatMessage(messages[hwDeviceStatus]);
    }

    return (
      <>
        <div className={componentClasses}>
          <div className={styles.messageWrapper}>
            <div className={styles.message}>
              {hasInstructionsLink && instructionsLink ? (
                <FormattedMessage
                  {...messages[hwDeviceStatus]}
                  values={{
                    instructionsLink,
                  }}
                />
              ) : (
                label
              )}
            </div>
            {hasPassphraseLabel && isTrezor && (
              <div className={styles.passphraseLabel}>
                {intl.formatMessage(messages.enterPassphrase)}
              </div>
            )}
          </div>
          {isLoading && (
            <LoadingSpinner className="hardwareWalletProcessProgress" />
          )}
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

export default HardwareWalletStatus;

// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import {
  defineMessages,
  intlShape,
  FormattedHTMLMessage,
  FormattedMessage,
} from 'react-intl';
import SVGInline from 'react-svg-inline';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { get } from 'lodash';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import ledgerIcon from '../../assets/images/hardware-wallet/ledger-cropped.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import ledgerXIcon from '../../assets/images/hardware-wallet/ledger-x-cropped.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import trezorIcon from '../../assets/images/hardware-wallet/trezor.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/hardware-w... Remove this comment to see the full error message
import unknownDeviceIcon from '../../assets/images/hardware-wallet/trezor-ledger.inline.svg';
import DialogCloseButton from '../widgets/DialogCloseButton';
import LocalizableError from '../../i18n/LocalizableError';
import Dialog from '../widgets/Dialog';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletConnectDialog.scss' or... Remove this comment to see the full error message
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
      '!!!<p>Daedalus currently supports Ledger Nano S, Ledger Nano X, and Trezor Model T hardware wallet devices.</p><p>If you are <b>pairing your device with Daedalus for the first time</b>, please follow the instructions below.</p><p>If you have <b>already paired your device with Daedalus</b>, you don’t need to repeat this step. Just connect your device when you need to confirm a transaction.</p>',
    description: 'Follow instructions label',
  },
  instructionsTrezorOnly: {
    id: 'wallet.connect.dialog.instructionsTrezorOnly',
    defaultMessage:
      '!!!<p><b>Daedalus currently supports only Trezor Model T hardware wallet devices.</b></p><p>If you are <b>pairing your device with Daedalus for the first time</b>, please follow the instructions below.</p><p>If you have <b>already paired your device with Daedalus</b>, you don’t need to repeat this step. Just connect your device when you need to confirm a transaction.</p>',
    description: 'Follow instructions label',
  },
  connectingIssueSupportLabel: {
    id: 'wallet.connect.dialog.connectingIssueSupportLabel',
    defaultMessage:
      '!!!If you are experiencing issues pairing your hardware wallet device, please {supportLink}',
    description: 'Connecting issue support description',
  },
  connectingIssueSupportLink: {
    id: 'wallet.connect.dialog.connectingIssueSupportLink',
    defaultMessage: '!!!read the instructions.',
    description: 'Connecting issue support link',
  },
  connectingIssueSupportLinkUrl: {
    id: 'wallet.connect.dialog.connectingIssueSupportLinkUrl',
    defaultMessage: 'https://support.ledger.com/hc/en-us/articles/115005165269',
    description: 'Link to support article',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  isSubmitting: boolean;
  hwDeviceStatus: HwDeviceStatus;
  transportDevice: TransportDevice | null | undefined;
  error: LocalizableError | null | undefined;
  onExternalLinkClick: (...args: Array<any>) => any;
};

@observer
class WalletConnectDialog extends Component<Props> {
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

    const instructions = isLedgerEnabled
      ? messages.instructions
      : messages.instructionsTrezorOnly;
    const supportLink = (
      <Link
        className={styles.externalLink}
        onClick={() =>
          onExternalLinkClick(
            intl.formatMessage(messages.connectingIssueSupportLinkUrl)
          )
        }
        label={intl.formatMessage(messages.connectingIssueSupportLink)}
        skin={LinkSkin}
      />
    );
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
                <FormattedHTMLMessage {...instructions} />
              </p>
              <div className={styles.hardwareWalletStatusWrapper}>
                <HardwareWalletStatus
                  hwDeviceStatus={hwDeviceStatus}
                  onExternalLinkClick={onExternalLinkClick}
                  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
                  isTransactionStatus={false}
                  isTrezor={isTrezor}
                />
              </div>
              <div className={styles.hardwareWalletIssueArticleWrapper}>
                <p>
                  <FormattedMessage
                    {...messages.connectingIssueSupportLabel}
                    values={{
                      supportLink,
                    }}
                  />
                </p>
              </div>
            </div>
          )}
        </div>
      </Dialog>
    );
  }
}

export default WalletConnectDialog;

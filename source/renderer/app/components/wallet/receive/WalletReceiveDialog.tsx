import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { map, filter } from 'lodash';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classnames from 'classnames';
import CopyToClipboard from 'react-copy-to-clipboard';
import SVGInline from 'react-svg-inline';
import { TextArea } from 'react-polymorph/lib/components/TextArea';
import { TextAreaSkin } from 'react-polymorph/lib/skins/simple/TextAreaSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import QRCode from 'qrcode.react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import RadioSet from '../../widgets/RadioSet';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import WalletAddress from '../../../domains/WalletAddress';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletReceiveDialog.scss' or... Remove this comment to see the full error message
import styles from './WalletReceiveDialog.scss';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clipboa... Remove this comment to see the full error message
import iconCopy from '../../../assets/images/clipboard-ic.inline.svg';
import { HW_SHELLEY_CONFIG } from '../../../config/hardwareWalletsConfig';
import { hardenedPathToDerivationPath } from '../../../utils/hardwareWalletUtils';
import { AddressVerificationCheckStatuses } from '../../../stores/HardwareWalletsStore';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { AddressVerificationCheckStatus } from '../../../stores/HardwareWalletsStore';
import type { HwDeviceStatus } from '../../../domains/Wallet';

const messages = defineMessages({
  inputLabel: {
    id: 'wallet.receive.dialog.inputLabel',
    defaultMessage: '!!!PDF note',
    description: 'placeholder on the wallet "Share Address" dialog',
  },
  inputPlaceholder: {
    id: 'wallet.receive.dialog.inputPlaceholder',
    defaultMessage: '!!!Add a note to the sender',
    description: 'inputPlaceholder on the wallet "Share Address" dialog',
  },
  saveQRCodeImage: {
    id: 'wallet.receive.dialog.saveQRCodeImage',
    defaultMessage: '!!!Save QR code image',
    description: 'saveQRCodeImage on the wallet "Share Address" dialog',
  },
  downloadPDFButton: {
    id: 'wallet.receive.dialog.downloadPDFButton',
    defaultMessage: '!!!Download as PDF',
    description: 'downloadPDFButton on the wallet "Share Address" dialog',
  },
  dialogTitle: {
    id: 'wallet.receive.dialog.dialogTitle',
    defaultMessage: '!!!Share wallet address',
    description: 'dialogTitle on the wallet "Share Address" dialog',
  },
  copyAddressLabel: {
    id: 'wallet.receive.dialog.copyAddressLabel',
    defaultMessage: '!!!Copy address',
    description: 'Label for "Copy address" link on the wallet "Receive page"',
  },
  spendingPathTooltip: {
    id: 'wallet.receive.dialog.spendingPathTooltip',
    defaultMessage: '!!!Receiving address path',
    description: 'Tooltip for the receiving address path',
  },
  stakingPathTooltip: {
    id: 'wallet.receive.dialog.stakingPathTooltip',
    defaultMessage: '!!!Rewards address path',
    description: 'Tooltip for the rewards address path',
  },
  supportRequestButtonLabel: {
    id: 'wallet.receive.dialog.supportRequestButtonLabel',
    defaultMessage: '!!!Submit a request to IOHK Support',
    description: 'Support request button label',
  },
  supportRequestLinkUrl: {
    id: 'wallet.receive.dialog.supportRequestLinkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Support request link URL',
  },
  invalidAddressConfirmationLabel: {
    id: 'wallet.receive.dialog.invalidAddressConfirmationLabel',
    defaultMessage:
      '!!!Yes, I am sure I have compared the address displayed in Daedalus with the address displayed on the {deviceType} device by comparing the beginning and ending of the address.',
    description: 'Invalid address confirmation checkbox label',
  },
  verificationCheckOptionsLabel: {
    id: 'wallet.receive.dialog.verificationCheckOptionsLabel',
    defaultMessage: '!!!Is the address you have verified correct?',
    description: 'Verification options section label',
  },
  verificationCheckOptionValid: {
    id: 'wallet.receive.dialog.verificationCheckOptionValid',
    defaultMessage: '!!!Yes',
    description: 'Verification option "Valid" label',
  },
  verificationCheckOptionInvalid: {
    id: 'wallet.receive.dialog.verificationCheckOptionInvalid',
    defaultMessage:
      '!!!No, I am sure that address displayed in Daedalus is different from the address displayed on my {deviceType} device',
    description: 'Verification option "Invalid" label',
  },
  verificationCheckOptionReverify: {
    id: 'wallet.receive.dialog.verificationCheckOptionReverify',
    defaultMessage: '!!!No, reverify',
    description: 'Verification option "Reverify" label',
  },
  softwareCheckLabel: {
    id: 'wallet.receive.dialog.softwareCheckLabel',
    defaultMessage: '!!!Daedalus verified the address',
    description: 'Daedalus verification status check label',
  },
  confirmationCheckLabel: {
    id: 'wallet.receive.dialog.confirmationCheckLabel',
    defaultMessage: '!!!You have verified the address',
    description: 'User verification status check label',
  },
  addressVerificationInstructions: {
    id: 'wallet.receive.dialog.addressVerificationInstructions',
    defaultMessage:
      '!!!Please compare the address displayed here on the screen with the address displayed on the {deviceType} device by comparing <b>at least the first 5 characters at the start</b> of the address after the "addr" part and <b>at least 5 characters at the end</b> of the address.',
    description: 'Address verification instructions',
  },
  invalidAddressWarningTitle: {
    id: 'wallet.receive.dialog.invalidAddressWarningTitle',
    defaultMessage: '!!!Warning, your copy of Daedalus may be hacked!',
    description: 'Invalid address "Warning" title',
  },
  invalidAddressWarningDescription: {
    id: 'wallet.receive.dialog.invalidAddressWarningDescription',
    defaultMessage:
      '!!!You have manually compared the address shown in Daedalus with the address shown on the hardware wallet device and reported that they are different. If this is the case, please contact support and make sure you download and attach logs.',
    description: 'Invalid address "Warning" description',
  },
});
messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  address: WalletAddress;
  isHardwareWallet: boolean;
  walletName: string;
  hwDeviceStatus: HwDeviceStatus;
  isAddressDerived: boolean;
  isAddressChecked: boolean;
  onCopyAddress: (...args: Array<any>) => any;
  onDownloadPDF: (...args: Array<any>) => any;
  onSaveQRCodeImage: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
  onChangeVerificationStatus: (...args: Array<any>) => any;
  onSupportRequestClick: (...args: Array<any>) => any;
  isTrezor: boolean;
};
type State = {
  selectedVerificationStatus: AddressVerificationCheckStatus | null | undefined;
  isInvalidAddressConfirmed: boolean;
  isReverifying: boolean;
};

@observer
class WalletReceiveDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    selectedVerificationStatus: null,
    isInvalidAddressConfirmed: false,
    isReverifying: false,
  };
  // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 1.
  form = new ReactToolboxMobxForm({
    fields: {
      noteInput: {
        value: '',
        label: this.context.intl.formatMessage(messages.inputLabel),
        placeholder: this.context.intl.formatMessage(messages.inputPlaceholder),
      },
    },
  });
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { noteInput } = form.values();
        const { onDownloadPDF } = this.props;
        onDownloadPDF(noteInput);
      },
      onError: (err) => {
        throw new Error(err);
      },
    });
  };
  handleChange = (field: { value: string }) => {
    field.value = field.value.replace(/\n/g, '');
  };
  constructPaths = (address: WalletAddress) => {
    const hardenedSpendingPath = utils.str_to_path(address.spendingPath);
    const derivationSpendingPath = hardenedPathToDerivationPath(
      hardenedSpendingPath
    );
    const spendingPath = map(
      derivationSpendingPath.constructed,
      (constructeSpendingPathChunk, index) => {
        const isChangeablePart =
          index >= derivationSpendingPath.constructed.length - 2;

        if (isChangeablePart) {
          return <b key={`chunk-${index}`}>/{constructeSpendingPathChunk}</b>;
        }

        return index === 0
          ? constructeSpendingPathChunk
          : `/${constructeSpendingPathChunk}`;
      }
    );
    const derivationStakingPath = hardenedPathToDerivationPath(
      HW_SHELLEY_CONFIG.DEFAULT_DERIVATION_PATH
    );
    const stakingPath = map(
      derivationStakingPath.constructed,
      (constructeStakingPathChunk, index) => {
        const isLastIndex =
          index === derivationStakingPath.constructed.length - 1;

        if (isLastIndex) {
          return <b key={`chunk-${index}`}>/{constructeStakingPathChunk}</b>;
        }

        return index === 0
          ? constructeStakingPathChunk
          : `/${constructeStakingPathChunk}`;
      }
    );
    return {
      stakingPath,
      spendingPath,
    };
  };
  onChangeVerificationStatus = (status: AddressVerificationCheckStatus) => {
    this.setState({
      selectedVerificationStatus:
        status === AddressVerificationCheckStatuses.REVERIFY ? null : status,
      isInvalidAddressConfirmed: false,
      isReverifying: status === AddressVerificationCheckStatuses.REVERIFY,
    });
    this.props.onChangeVerificationStatus(status);
  };
  handleConfirmInvalidAddress = (isConfirmed: boolean) => {
    this.setState({
      isInvalidAddressConfirmed: isConfirmed,
    });
  };

  render() {
    const {
      address,
      onCopyAddress,
      onSaveQRCodeImage,
      onClose,
      walletName,
      hwDeviceStatus,
      isHardwareWallet,
      isAddressDerived,
      isAddressChecked,
      onSupportRequestClick,
      isTrezor,
    } = this.props;
    const {
      selectedVerificationStatus,
      isInvalidAddressConfirmed,
      isReverifying,
    } = this.state;
    const { intl } = this.context;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const noteInputField = this.form.$('noteInput');
    const deviceType = isHardwareWallet && isTrezor ? 'Trezor' : 'Ledger';
    const isSubmitting = false;
    const buttonClasses = classnames([
      'attention',
      isSubmitting ? styles.isSubmitting : null,
    ]);
    const supportButtonLabel = !isSubmitting ? (
      intl.formatMessage(messages.supportRequestButtonLabel)
    ) : (
      <LoadingSpinner />
    );
    const isSupportRequestButton =
      selectedVerificationStatus === AddressVerificationCheckStatuses.INVALID;
    let actions;

    if (isSupportRequestButton) {
      const supportRequestLinkUrl = intl.formatMessage(
        messages.supportRequestLinkUrl
      );
      actions = [
        {
          className: buttonClasses,
          label: supportButtonLabel,
          onClick: onSupportRequestClick.bind(this, supportRequestLinkUrl),
          disabled: !isInvalidAddressConfirmed,
          primary: true,
        },
      ];
    } else {
      actions = [
        {
          label: intl.formatMessage(messages.saveQRCodeImage),
          onClick: () => onSaveQRCodeImage(),
        },
        {
          className: 'downloadPDFButton',
          label: intl.formatMessage(messages.downloadPDFButton),
          onClick: this.submit,
          primary: true,
        },
      ];
    }

    // Get QRCode color value from active theme's CSS variable
    const qrCodeBackgroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-background-color'
        )
      : 'transparent';
    const qrCodeForegroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-foreground-color'
        )
      : '#000';
    const constructedPaths = this.constructPaths(address);
    const verificationOptions = [
      {
        status: AddressVerificationCheckStatuses.VALID,
        label: intl.formatMessage(messages.verificationCheckOptionValid),
      },
      {
        status: AddressVerificationCheckStatuses.REVERIFY,
        label: intl.formatMessage(messages.verificationCheckOptionReverify),
      },
      {
        status: AddressVerificationCheckStatuses.INVALID,
        label: intl.formatMessage(messages.verificationCheckOptionInvalid, {
          deviceType,
        }),
      },
    ];
    const filteredVerificationOptions = filter(
      verificationOptions,
      (option) => {
        const isInvalidOption =
          option.status === AddressVerificationCheckStatuses.INVALID;

        if (
          (!selectedVerificationStatus &&
            (!isInvalidOption || (isInvalidOption && isReverifying))) ||
          (selectedVerificationStatus &&
            selectedVerificationStatus === option.status)
        ) {
          return option;
        }

        return null;
      }
    );
    const showActions =
      !isHardwareWallet ||
      (isHardwareWallet &&
        (selectedVerificationStatus ===
          AddressVerificationCheckStatuses.INVALID ||
          selectedVerificationStatus ===
            AddressVerificationCheckStatuses.VALID));
    const isAddressConfirmed =
      isAddressChecked &&
      isAddressDerived &&
      selectedVerificationStatus !== null;
    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        subtitle={walletName}
        actions={showActions ? actions : []}
        closeOnOverlayClick={false}
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <div className={styles.container}>
          <div className={styles.qrCode}>
            <QRCode
              value={address.id}
              bgColor={qrCodeBackgroundColor}
              fgColor={qrCodeForegroundColor}
              size={192}
            />
          </div>

          <div className={styles.addressPathsWrapper}>
            <PopOver content={intl.formatMessage(messages.spendingPathTooltip)}>
              <div className={styles.spendingPath}>
                {constructedPaths.spendingPath}
              </div>
            </PopOver>

            <PopOver content={intl.formatMessage(messages.stakingPathTooltip)}>
              <div className={styles.stakingPath}>
                {constructedPaths.stakingPath}
              </div>
            </PopOver>
          </div>

          <div className={styles.address}>{address.id}</div>

          <CopyToClipboard
            text={address.id}
            onCopy={() => onCopyAddress(address.id)}
          >
            <span className={styles.copyAddress}>
              <SVGInline svg={iconCopy} className={styles.copyIcon} />
              <span className={styles.copyAddressLabel}>
                {intl.formatMessage(messages.copyAddressLabel)}
              </span>
            </span>
          </CopyToClipboard>

          {isHardwareWallet && (
            <div className={styles.hardwareWalletStatusWrapper}>
              <HardwareWalletStatus
                hwDeviceStatus={hwDeviceStatus}
                walletName={walletName}
                isTrezor={isTrezor}
              />

              <div className={styles.verificationCheckboxes}>
                <Checkbox
                  checked={isAddressDerived}
                  label={intl.formatMessage(messages.softwareCheckLabel)}
                  disabled
                  skin={CheckboxSkin}
                />
                <Checkbox
                  checked={isAddressConfirmed}
                  label={intl.formatMessage(messages.confirmationCheckLabel)}
                  disabled
                  skin={CheckboxSkin}
                />
              </div>

              <p className={styles.verificationInstructions}>
                <FormattedHTMLMessage
                  {...messages.addressVerificationInstructions}
                  values={{
                    deviceType,
                  }}
                />
              </p>

              {isAddressDerived && isAddressChecked && (
                <RadioSet
                  label={
                    <p>
                      {intl.formatMessage(
                        messages.verificationCheckOptionsLabel
                      )}
                    </p>
                  }
                  items={map(filteredVerificationOptions, (option) => ({
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                    key: option.status,
                    disabled: false,
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'label' does not exist on type 'number | ... Remove this comment to see the full error message
                    label: option.label,
                    // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                    selected: option.status === selectedVerificationStatus,
                    onChange: () =>
                      // @ts-ignore ts-migrate(2339) FIXME: Property 'status' does not exist on type 'number |... Remove this comment to see the full error message
                      this.onChangeVerificationStatus(option.status),
                  }))}
                  verticallyAligned
                />
              )}
            </div>
          )}

          {selectedVerificationStatus ===
            AddressVerificationCheckStatuses.INVALID && (
            <div className={styles.warningWrapper}>
              <Checkbox
                label={intl.formatMessage(
                  messages.invalidAddressConfirmationLabel,
                  {
                    deviceType,
                  }
                )}
                onChange={this.handleConfirmInvalidAddress}
                checked={isInvalidAddressConfirmed}
                skin={CheckboxSkin}
              />
              <div className={styles.warningDescriptionWrapper}>
                <p className={styles.warningTitle}>
                  {intl.formatMessage(messages.invalidAddressWarningTitle)}
                </p>
                <p className={styles.warningDescription}>
                  {intl.formatMessage(
                    messages.invalidAddressWarningDescription,
                    {
                      deviceType,
                    }
                  )}
                </p>
              </div>
            </div>
          )}
          {(!isHardwareWallet ||
            selectedVerificationStatus ===
              AddressVerificationCheckStatuses.VALID) && (
            <TextArea
              className={styles.noteInput}
              skin={TextAreaSkin}
              autoResize={false}
              rows={3}
              maxLength={201}
              {...noteInputField.bind({
                onChange: this.handleChange(noteInputField),
              })}
            />
          )}
        </div>
      </Dialog>
    );
  }
}

export default WalletReceiveDialog;

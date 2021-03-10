// @flow
import React, { Component, Fragment } from 'react';
import { observer } from 'mobx-react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import { get } from 'lodash';
import BigNumber from 'bignumber.js';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import globalMessages from '../../../i18n/global-messages';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletSendAssetsConfirmationDialog.scss';
import questionMarkIcon from '../../../assets/images/question-mark.inline.svg';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import { HwDeviceStatuses } from '../../../domains/Wallet';
import AssetToken from '../../widgets/AssetToken';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import type { WalletSummaryAsset } from '../../../api/assets/types';

const SHOW_TOTAL_AMOUNT = false;

export const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.send.confirmationDialog.title',
    defaultMessage: '!!!Confirm transaction',
    description: 'Title for the "Confirm transaction" dialog.',
  },
  passphraseLabel: {
    id: 'wallet.send.confirmationDialog.passphraseLabel',
    defaultMessage: '!!!Spending password',
    description:
      'Label for the "Spending password" input in the wallet send confirmation dialog.',
  },
  addressToLabel: {
    id: 'wallet.send.confirmationDialog.addressToLabel',
    defaultMessage: '!!!To',
    description: 'Label for the "To" in the wallet send confirmation dialog.',
  },
  amountLabel: {
    id: 'wallet.send.confirmationDialog.amountLabel',
    defaultMessage: '!!!Amount',
    description:
      'Label for the "Amount" in the wallet send confirmation dialog.',
  },
  assetLabel: {
    id: 'wallet.send.confirmationDialog.assetLabel',
    defaultMessage: '!!!Token',
    description: 'Token',
  },
  feesLabel: {
    id: 'wallet.send.confirmationDialog.feesLabel',
    defaultMessage: '!!!Transaction fee',
    description: 'Label for the "Fees" in the wallet send confirmation dialog.',
  },
  totalLabel: {
    id: 'wallet.send.confirmationDialog.totalLabel',
    defaultMessage: '!!!Total',
    description:
      'Label for the "Total" in the wallet send confirmation dialog.',
  },
  receiverLabel: {
    id: 'wallet.send.confirmationDialog.receiver.label',
    defaultMessage: '!!!Receiver',
    description:
      'Label for the "Receiver" in the wallet send confirmation dialog.',
  },
  passphraseFieldPlaceholder: {
    id: 'wallet.send.confirmationDialog.passphraseFieldPlaceholder',
    defaultMessage: '!!!Type your spending password',
    description:
      'Placeholder for the "Spending password" inputs in the wallet send confirmation dialog.',
  },
  flightCandidateWarning: {
    id: 'wallet.send.confirmationDialog.flightCandidateWarning',
    defaultMessage:
      '!!!{Warning}, flight candidate versions of Daedalus are connected to Cardano mainnet. If you confirm this transaction, your ada will be sent for real.',
    description:
      'Text for the "Flight candidate" warning in the wallet send confirmation dialog.',
  },
  flightCandidateCheckboxLabel: {
    id: 'wallet.send.confirmationDialog.flightCandidateCheckboxLabel',
    defaultMessage:
      '!!!I understand that real ada will be moved as part of this transaction and that this action is irreversible.',
    description:
      'Label for the "Flight candidate" warning checkbox in the wallet send confirmation dialog.',
  },
  sendButtonLabel: {
    id: 'wallet.send.confirmationDialog.submit',
    defaultMessage: '!!!Send',
    description:
      'Label for the send button in the wallet send confirmation dialog.',
  },
  backButtonLabel: {
    id: 'wallet.send.confirmationDialog.back',
    defaultMessage: '!!!Back',
    description:
      'Label for the back button in the wallet send confirmation dialog.',
  },
  passwordErrorMessage: {
    id: 'wallet.send.confirmationDialog.passwordError',
    defaultMessage: '!!!Incorrect spending password.',
    description:
      'Label for password error in the wallet send confirmation dialog.',
  },
  unformattedAmountLabel: {
    id: 'wallet.send.confirmationDialog.unformattedAmountLabel',
    defaultMessage: '!!!unformatted amount',
    description: 'Label for "unformated amount"',
  },
  unformattedAmountMessageForSoftwareWallets: {
    id:
      'wallet.send.confirmationDialog.unformattedAmountMessageForSoftwareWallets',
    defaultMessage:
      '!!!The native token unformatted amount (amount without decimal places) will be displayed on the hardware wallet device during transaction confirmation.',
    description: 'Message for "unformated amount"',
  },
  unformattedAmountMessageForHardwareWallets: {
    id:
      'wallet.send.confirmationDialog.unformattedAmountMessageForHardwareWallets',
    defaultMessage:
      '!!!The native token unformatted amount (amount without decimal places) will be displayed on the hardware wallet device during transaction confirmation.',
    description: 'Message for "unformated amount"',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  amount: string,
  totalAmount: ?string,
  receiver: string,
  assets: Array<WalletSummaryAsset>,
  assetsAmounts: Array<string>,
  transactionFee: ?string,
  onSubmit: Function,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  onCancel: Function,
  onExternalLinkClick: Function,
  isSubmitting: boolean,
  isFlight: boolean,
  error: ?LocalizableError,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
  onInitiateTransaction: Function,
  walletName: string,
  onExternalLinkClick: Function,
  onCopyAssetItem: Function,
  currencyUnit: string,
};

type State = {
  areTermsAccepted: boolean,
};

@observer
export default class WalletSendAssetsConfirmationDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    areTermsAccepted: false,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        passphrase: {
          type: 'password',
          label: this.context.intl.formatMessage(messages.passphraseLabel),
          placeholder: this.context.intl.formatMessage(
            messages.passphraseFieldPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              if (this.props.isHardwareWallet) return [true];
              if (field.value === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
        flightCandidateCheckbox: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(
            messages.flightCandidateCheckboxLabel
          ),
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const {
          receiver,
          amount,
          assets,
          assetsAmounts,
          amountToNaturalUnits,
          isHardwareWallet,
        } = this.props;
        const { passphrase } = form.values();
        const transactionData = {
          receiver,
          amount: amountToNaturalUnits(amount),
          passphrase,
          isHardwareWallet,
          assets,
          assetsAmounts,
        };
        this.props.onSubmit(transactionData);
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = (event: {}) =>
    (this.props.isHardwareWallet || this.form.$('passphrase').isValid) &&
    submitOnEnter(this.submit, event);

  renderConfirmationElement = (isHardwareWallet: boolean) => {
    const passphraseField = this.form.$('passphrase');
    const { areTermsAccepted } = this.state;
    const {
      hwDeviceStatus,
      isFlight,
      onExternalLinkClick,
      walletName,
    } = this.props;

    if (!isFlight || (isFlight && areTermsAccepted)) {
      if (isHardwareWallet) {
        return (
          <div className={styles.hardwareWalletStatusWrapper}>
            <HardwareWalletStatus
              hwDeviceStatus={hwDeviceStatus}
              walletName={walletName}
              onExternalLinkClick={onExternalLinkClick}
            />
          </div>
        );
      }
      return (
        <Input
          type="password"
          className={styles.passphrase}
          {...passphraseField.bind()}
          error={passphraseField.error}
          skin={InputSkin}
          onKeyPress={this.handleSubmitOnEnter}
          autoFocus
        />
      );
    }
    return null;
  };

  onCheckboxClick = (areTermsAccepted: boolean) => {
    const { isHardwareWallet, onInitiateTransaction } = this.props;
    this.setState({ areTermsAccepted });
    if (isHardwareWallet) {
      onInitiateTransaction();
    }
  };

  getAssetAmount = (index: number) => {
    const { assetsAmounts } = this.props;
    const asset = get(assetsAmounts, index, 0);
    return asset;
  };

  getFormattedAssetAmount = (
    { metadata }: WalletSummaryAsset,
    index: number
  ) => {
    const assetAmount = this.getAssetAmount(index);
    return formattedTokenWalletAmount(new BigNumber(assetAmount), metadata);
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { areTermsAccepted } = this.state;
    const passphraseField = form.$('passphrase');
    const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
    const {
      onCancel,
      assets,
      amount,
      receiver,
      totalAmount,
      transactionFee,
      isSubmitting,
      isFlight,
      error,
      onExternalLinkClick,
      hwDeviceStatus,
      isHardwareWallet,
      onCopyAssetItem,
      currencyUnit,
    } = this.props;

    const buttonLabel = !isSubmitting ? (
      intl.formatMessage(messages.sendButtonLabel)
    ) : (
      <LoadingSpinner />
    );

    const actions = [
      {
        label: intl.formatMessage(messages.backButtonLabel),
        onClick: !isSubmitting ? onCancel : () => {},
      },
      {
        label: buttonLabel,
        onClick: this.submit,
        primary: true,
        className: 'confirmButton',
        disabled:
          (!isHardwareWallet && !passphraseField.isValid) ||
          (isHardwareWallet &&
            hwDeviceStatus !==
              HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED) ||
          (!areTermsAccepted && isFlight),
      },
    ];

    const assetsSeparatorBasicHeight = 27;
    const assetsSeparatorCalculatedHeight = assets.length
      ? assetsSeparatorBasicHeight * (assets.length * 2) - 18
      : assetsSeparatorBasicHeight;

    let errorElement = null;
    if (error) {
      const errorHasLink = !!error.values.linkLabel;
      errorElement = errorHasLink ? (
        <FormattedHTMLMessageWithLink
          message={error}
          onExternalLinkClick={onExternalLinkClick}
        />
      ) : (
        intl.formatMessage(error)
      );
    }

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        primaryButtonAutoFocus
        onClose={!isSubmitting ? onCancel : () => {}}
        className={styles.dialog}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.passphraseFields}>
          <div className={styles.addressToLabelWrapper}>
            <div className={styles.receiverRow}>
              <div className={styles.receiverRowItem}>
                <h2>{intl.formatMessage(messages.receiverLabel)}</h2>
                <div className={styles.receiverRowItemAddresses}>
                  <p className={styles.addressTo}>{receiver}</p>
                  <div className={styles.assetsWrapper}>
                    <div
                      className={styles.assetsSeparator}
                      style={{
                        height: `${assetsSeparatorCalculatedHeight}px`,
                        top: `${assetsSeparatorCalculatedHeight + 5}px`,
                        marginTop: `-${assetsSeparatorCalculatedHeight + 5}px`,
                      }}
                    />
                    <div className={styles.assetsContainer}>
                      <h3>
                        <span>{currencyUnit}</span>
                      </h3>
                      <div className={styles.amountFeesWrapper}>
                        <div className={styles.amount}>
                          {amount} {currencyUnit}
                        </div>
                      </div>
                    </div>
                    {assets.map((asset, assetIndex) => {
                      const assetAmount = this.getFormattedAssetAmount(
                        asset,
                        assetIndex
                      );
                      return (
                        <Fragment key={asset.fingerprint}>
                          <div className={styles.assetsContainer}>
                            <h3>
                              <span>
                                {intl.formatMessage(messages.assetLabel)}
                                &nbsp;#{assetIndex + 1}
                              </span>
                              <AssetToken
                                asset={asset}
                                onCopyAssetItem={onCopyAssetItem}
                                componentClassName={styles.assetToken}
                              />
                            </h3>
                            <div className={styles.amountFeesWrapper}>
                              <div className={styles.amount}>{assetAmount}</div>
                            </div>
                          </div>
                          <div className={styles.assetsContainer}>
                            <div className={styles.unformattedAmountLine} />
                            <div className={styles.unformattedAmountLabel}>
                              {intl.formatMessage(
                                messages.unformattedAmountLabel
                              )}
                              <PopOver
                                content={intl.formatMessage(
                                  isHardwareWallet
                                    ? messages.unformattedAmountMessageForHardwareWallets
                                    : messages.unformattedAmountMessageForSoftwareWallets
                                )}
                                key="tooltip"
                              >
                                <div className={styles.questionMark}>
                                  <SVGInline svg={questionMarkIcon} />
                                </div>
                              </PopOver>
                              {':'}
                            </div>
                            <div className={styles.unformattedAmount}>
                              {this.getAssetAmount(assetIndex)}
                            </div>
                          </div>
                        </Fragment>
                      );
                    })}
                  </div>
                </div>
              </div>
            </div>
          </div>

          {SHOW_TOTAL_AMOUNT ? (
            <>
              <div className={styles.adaAmountFeesWrapper}>
                <div className={styles.adaAmountWrapper}>
                  <div className={styles.adaAmountLabel}>
                    {intl.formatMessage(messages.amountLabel)}
                  </div>
                  <div className={styles.adaAmount}>
                    {amount}
                    <span className={styles.currencySymbol}>
                      &nbsp;{currencyUnit}
                    </span>
                  </div>
                </div>

                <div className={styles.feesWrapper}>
                  <div className={styles.feesLabel}>
                    {intl.formatMessage(messages.feesLabel)}
                  </div>
                  <div className={styles.fees}>
                    +{transactionFee}
                    <span className={styles.currencySymbol}>
                      &nbsp;{currencyUnit}
                    </span>
                  </div>
                </div>
              </div>

              <div className={styles.totalAmountWrapper}>
                <div className={styles.totalAmountLabel}>
                  {intl.formatMessage(messages.totalLabel)}
                </div>
                <div className={styles.totalAmount}>
                  {totalAmount}
                  <span className={styles.currencySymbol}>
                    &nbsp;{currencyUnit}
                  </span>
                </div>
              </div>
            </>
          ) : (
            <div className={styles.feesWrapper}>
              <div className={styles.feesLabel}>
                {intl.formatMessage(messages.feesLabel)}
              </div>
              <div className={styles.fees}>
                +{transactionFee}
                <span className={styles.currencySymbol}>
                  &nbsp;{currencyUnit}
                </span>
              </div>
            </div>
          )}

          {isFlight && (
            <div className={styles.flightCandidateWarning}>
              <FormattedHTMLMessage
                {...messages.flightCandidateWarning}
                tagName="p"
              />
              <Checkbox
                {...flightCandidateCheckboxField.bind()}
                error={flightCandidateCheckboxField.error}
                skin={CheckboxSkin}
                disabled={areTermsAccepted}
                onChange={this.onCheckboxClick}
                checked={areTermsAccepted}
              />
            </div>
          )}

          {this.renderConfirmationElement(isHardwareWallet)}
        </div>

        {errorElement ? <p className={styles.error}>{errorElement}</p> : null}
      </Dialog>
    );
  }
}

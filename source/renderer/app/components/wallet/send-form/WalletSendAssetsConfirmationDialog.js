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
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './WalletSendAssetsConfirmationDialog.scss';
import questionMarkIcon from '../../../assets/images/question-mark.inline.svg';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import { formattedTokenWalletAmount } from '../../../utils/formatters';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import Asset from '../../assets/Asset';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import type { AssetToken } from '../../../api/assets/types';
import { getMessages } from './WalletSendAssetsConfirmationDialog.messages';
import { shouldShowEmptyWalletWarning } from '../../../utils/walletUtils';

const SHOW_TOTAL_AMOUNT = false;

type Props = {
  amount: string,
  receiver: string,
  wallet: Wallet,
  totalAmount: BigNumber,
  assets: Array<AssetToken>,
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
  onCopyAssetItem: Function,
  currencyUnit: string,
  isTrezor: boolean,
  formattedTotalAmount: string,
};

type State = {
  assets: Array<AssetToken>,
  assetsAmounts: Array<string>,
  areTermsAccepted: boolean,
};

const messages = getMessages();

@observer
export default class WalletSendAssetsConfirmationDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    assets: [],
    assetsAmounts: [],
    areTermsAccepted: false,
  };

  componentDidMount() {
    // We need to keep initial list of assets and their amounts as a state
    // value to avoid losing them after the transaction is confirmed
    // (this affects only hardware wallets for which we close the dialog
    // after transaction has been confirmed)
    const { assets, assetsAmounts } = this.props;
    this.setState({ assets, assetsAmounts });
  }

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
        const { assets, assetsAmounts } = this.state;
        const {
          receiver,
          amount,
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

  renderConfirmationElement = (
    isHardwareWallet: boolean
  ): React$Element<*> | null => {
    const passphraseField = this.form.$('passphrase');
    const { areTermsAccepted } = this.state;
    const {
      hwDeviceStatus,
      isFlight,
      totalAmount,
      onExternalLinkClick,
      wallet,
      isTrezor,
    } = this.props;

    let returnJSX = null;
    if (!isFlight || (isFlight && areTermsAccepted)) {
      const walletWarning = (
        <div className={styles.flightCandidateWarning}>
          <FormattedHTMLMessage {...messages.emptyingWarning} tagName="p" />
        </div>
      );
      const { name, isLegacy } = wallet;
      returnJSX = isHardwareWallet ? (
        <>
          <div className={styles.hardwareWalletStatusWrapper}>
            <HardwareWalletStatus
              hwDeviceStatus={hwDeviceStatus}
              walletName={name}
              isTrezor={isTrezor}
              onExternalLinkClick={onExternalLinkClick}
            />
          </div>
          {!isLegacy &&
            shouldShowEmptyWalletWarning(totalAmount, wallet) &&
            walletWarning}
        </>
      ) : (
        <>
          <Input
            type="password"
            className={styles.passphrase}
            {...passphraseField.bind()}
            error={passphraseField.error}
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
            autoFocus
          />
          {!isLegacy &&
            shouldShowEmptyWalletWarning(totalAmount, wallet) &&
            walletWarning}
        </>
      );
    }
    return returnJSX;
  };

  onCheckboxClick = (areTermsAccepted: boolean) => {
    const { isHardwareWallet, onInitiateTransaction } = this.props;
    this.setState({ areTermsAccepted });
    if (isHardwareWallet) {
      onInitiateTransaction();
    }
  };

  getAssetAmount = (index: number) => {
    const { assetsAmounts } = this.state;
    const asset = get(assetsAmounts, index, 0);
    return asset;
  };

  getFormattedAssetAmount = (
    { metadata, decimals }: AssetToken,
    index: number
  ) => {
    const assetAmount = this.getAssetAmount(index);
    return formattedTokenWalletAmount(
      new BigNumber(assetAmount),
      metadata,
      decimals
    );
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { assets, areTermsAccepted } = this.state;
    const passphraseField = form.$('passphrase');
    const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
    const {
      onCancel,
      amount,
      receiver,
      transactionFee,
      isSubmitting,
      isFlight,
      error,
      onExternalLinkClick,
      hwDeviceStatus,
      isHardwareWallet,
      onCopyAssetItem,
      currencyUnit,
      wallet,
      formattedTotalAmount,
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
      ? assetsSeparatorBasicHeight * assets.length * 2 - 18
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
    const { name } = wallet;
    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        subtitle={name}
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
                        <Fragment key={asset.uniqueId}>
                          <div className={styles.assetsContainer}>
                            <h3>
                              <span>
                                {intl.formatMessage(messages.assetLabel)}
                                &nbsp;#{assetIndex + 1}
                              </span>
                              <Asset
                                asset={asset}
                                onCopyAssetItem={onCopyAssetItem}
                                className={styles.assetToken}
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
                                content={
                                  <div className="UnformattedAmountTooltip">
                                    <FormattedHTMLMessage
                                      {...getMessages()[
                                        isHardwareWallet
                                          ? 'unformattedAmountMessageForHardwareWallets'
                                          : 'unformattedAmountMessageForSoftwareWallets'
                                      ]}
                                      tagName="div"
                                    />
                                  </div>
                                }
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
                    <span className={styles.currencyCode}>
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
                    <span className={styles.currencyCode}>
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
                  {formattedTotalAmount}
                  <span className={styles.currencyCode}>
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
                <span className={styles.currencyCode}>
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

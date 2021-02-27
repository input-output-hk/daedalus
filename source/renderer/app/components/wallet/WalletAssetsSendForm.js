// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { intlShape } from 'react-intl';
import { filter, findIndex, get, omit, orderBy, map, without } from 'lodash';
import BigNumber from 'bignumber.js';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import AmountInputSkin from './skins/AmountInputSkin';
import BorderedBox from '../widgets/BorderedBox';
import LoadingSpinner from '../widgets/LoadingSpinner';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import { FormattedHTMLMessageWithLink } from '../widgets/FormattedHTMLMessageWithLink';
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
import closeIcon from '../../assets/images/close-cross.inline.svg';
import globalMessages from '../../i18n/global-messages';
import messages from './send-form/messages';
/* eslint-disable consistent-return */
import { messages as apiErrorMessages } from '../../api/errors';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../utils/form';
import {
  formattedAmountToNaturalUnits,
  formattedAmountToLovelace,
  formattedWalletAmount,
  formattedTokenWalletAmount,
} from '../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { TRANSACTION_MIN_ADA_VALUE } from '../../config/walletsConfig';
import { NUMBER_FORMATS } from '../../../../common/types/number.types';
import WalletAssetsSendConfirmationDialog from './WalletAssetsSendConfirmationDialog';
import WalletSendConfirmationDialogContainer from '../../containers/wallet/dialogs/WalletSendConfirmationDialogContainer';
import styles from './WalletAssetsSendForm.scss';
import Asset from '../../domains/Asset';
import type { HwDeviceStatus } from '../../domains/Wallet';
import type { AssetItems, WalletSummaryAsset } from '../../api/assets/types';

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  currencyUnit: string,
  currencyMaxIntegerDigits?: number,
  currencyMaxFractionalDigits: number,
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>,
  calculateTransactionFee: (
    address: string,
    amount: number,
    assets: AssetItems
  ) => Promise<BigNumber>,
  currentNumberFormat: string,
  walletAmount: BigNumber,
  addressValidator: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
  onExternalLinkClick?: Function,
  isRestoreActive: boolean,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
  isLoadingAssets: boolean,
  assets: Array<WalletSummaryAsset>,
  isClearTooltipOpeningDownward?: boolean,
  hasAssets: boolean,
  selectedAsset: ?Asset,
  unsetActiveAssetFingerprint: Function,
};

type State = {
  formFields: Object,
  minimumAda: BigNumber,
  transactionFee: BigNumber,
  feeCalculationRequestQue: number,
  transactionFeeError: ?string | ?Node,
  assetErrors: {
    [key: string]: ?string | ?Node,
  },
  showRemoveAssetButton: {
    [key: string]: boolean,
  },
  selectedAssetFingerprints: Array<string>,
  isResetButtonDisabled: boolean,
  isReceiverAddressValid: boolean,
  isTransactionFeeCalculated: boolean,
};

@observer
export default class WalletAssetsSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    formFields: {},
    isResetButtonDisabled: true,
    isReceiverAddressValid: false,
    isTransactionFeeCalculated: false,
    minimumAda: new BigNumber(0),
    transactionFee: new BigNumber(0),
    transactionFeeError: null,
    feeCalculationRequestQue: 0,
    assetErrors: {},
    showRemoveAssetButton: {},
    selectedAssetFingerprints: [],
  };

  // We need to track the fee calculation state in order to disable
  // the "Submit" button as soon as either receiver or amount field changes.
  // This is required as we are using debounced validation and we need to
  // disable the "Submit" button as soon as the value changes and then wait for
  // the validation to end in order to see if the button should be enabled or not.
  _isCalculatingTransactionFee = false;

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    this.updateFormFields(true);
    const { selectedAsset } = this.props;
    if (selectedAsset) {
      setTimeout(() => {
        if (this._isMounted) {
          this.addAssetRow(selectedAsset.fingerprint);
        }
      });
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.props.unsetActiveAssetFingerprint();
  }

  getCurrentNumberFormat() {
    return NUMBER_FORMATS[this.props.currentNumberFormat];
  }

  get selectedAssets() {
    const { selectedAssetFingerprints } = this.state;
    const { assets: allAssets } = this.props;
    return map(selectedAssetFingerprints, (fingerprint) =>
      allAssets.find((asset) => asset.fingerprint === fingerprint)
    );
  }

  get selectedAssetsAmounts() {
    const { selectedAssetFingerprints, formFields } = this.state;
    const assetFields = get(formFields, 'receiver.assetFields');
    return map(
      selectedAssetFingerprints,
      (fingerprint) => assetFields[fingerprint].value
    );
  }

  get availableAssets() {
    const { assets: allAssets } = this.props;
    const { selectedAssetFingerprints } = this.state;
    return filter(
      allAssets,
      ({ fingerprint }) => !selectedAssetFingerprints.includes(fingerprint)
    );
  }

  get hasAvailableAssets() {
    return this.availableAssets.length > 0;
  }

  getAssetByFingerprint = (fingerprint: string): ?WalletSummaryAsset => {
    const { assets: allAssets } = this.props;
    return allAssets.find((asset) => asset.fingerprint === fingerprint);
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.handleOnSubmit);

  handleOnSubmit = () => {
    if (this.isDisabled()) {
      return false;
    }
    this.props.openDialogAction({
      dialog: WalletAssetsSendConfirmationDialog,
    });
  };

  handleOnReset = () => {
    // Cancel all debounced field validations
    this.form.each((field) => {
      field.debouncedValidation.cancel();
    });
    this.form.reset();
    this.form.showErrors(false);
    this.disableResetButton();
    map(this.state.formFields.receiver.assetFields, (asset) => {
      this.clearAssetFieldValue(asset);
    });
    this.clearReceiverFieldValue();
    this.clearAdaAmountFieldValue();
    this.updateFormFields(true);
  };

  disableResetButton = () => {
    this.setState({
      isResetButtonDisabled: true,
    });
  };

  clearReceiverFieldValue = () => {
    const receiverField = this.form.$('receiver');
    if (receiverField) {
      receiverField.clear();
      this.setReceiverValidity(true);
    }
  };

  clearAdaAmountFieldValue = () => {
    const adaAmountField = this.form.$('adaAmount');
    if (adaAmountField) {
      adaAmountField.clear();
    }
  };

  clearAssetFieldValue = (asset: any) => {
    const assetField = asset || this.form.$('assetField');
    if (assetField) {
      assetField.clear();
    }
  };

  updateFormFields = (resetFormFields: boolean, fingerprint?: ?string) => {
    const formFields = this.form.fields;
    const receiverField = formFields.get('receiver');
    const adaAmountField = formFields.get('adaAmount');
    if (resetFormFields) {
      this.setState({
        minimumAda: new BigNumber(0),
        showRemoveAssetButton: {},
        selectedAssetFingerprints: [],
        formFields: {
          receiver: {
            receiver: receiverField,
            adaAmount: adaAmountField,
            assetFields: {},
            assetsDropdown: {},
          },
        },
      });
    } else {
      const { assetFields, assetsDropdown } = this.state.formFields.receiver;
      const assetField = fingerprint
        ? formFields.get(`asset_${fingerprint}`)
        : null;
      const assetsDropdownField = fingerprint
        ? formFields.get(`assetsDropdown_${fingerprint}`)
        : null;
      if (assetField) {
        assetFields[fingerprint] = assetField;
      }
      if (assetsDropdownField) {
        assetsDropdown[fingerprint] = assetsDropdownField;
      }
      this.setState((prevState) => ({
        formFields: {
          ...prevState.formFields,
          receiver: {
            ...prevState.formFields.receiver,
            assetFields,
            assetsDropdown,
          },
        },
      }));
    }
  };

  hasReceiverValue = () => {
    const receiverField = this.form.$('receiver');
    return receiverField.value.length > 0;
  };

  hasAssetValue = (asset: any) => {
    return get(asset, 'value', false);
  };

  isDisabled = () =>
    this._isCalculatingTransactionFee || !this.state.isTransactionFeeCalculated;

  form = new ReactToolboxMobxForm(
    {
      fields: {
        receiver: {
          label: this.context.intl.formatMessage(messages.receiverLabel),
          placeholder: this.context.intl.formatMessage(messages.receiverHint),
          value: '',
          validators: [
            async ({ field }) => {
              const { value } = field;
              if (value === '') {
                this.resetTransactionFee();
                this.setReceiverValidity(false);
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              const isValidAddress = await this.props.addressValidator(value);
              this.setReceiverValidity(isValidAddress);
              return [
                isValidAddress,
                this.context.intl.formatMessage(
                  apiErrorMessages.invalidAddress
                ),
              ];
            },
          ],
        },
        adaAmount: {
          label: `${this.context.intl.formatMessage(messages.assetAdaLabel)}`,
          placeholder: `0${
            this.getCurrentNumberFormat().decimalSeparator
          }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
          value: '',
          validators: [
            async ({ field, form }) => {
              const { isReceiverAddressValid } = this.state;
              if (field.value === null) {
                this.resetTransactionFee();
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              if (isReceiverAddressValid && field.value === '') {
                this.resetTransactionFee();
                return [
                  true,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              const amountValue = field.value.toString();
              const isValid = await this.props.validateAmount(
                formattedAmountToNaturalUnits(amountValue)
              );
              const receiverField = form.$('receiver');
              const receiverValue = receiverField.value;
              const isReceiverValid = receiverField.isValid;
              if (isValid && isReceiverValid) {
                const assets = this.selectedAssets.map(
                  ({ policyId, assetName }, index) => {
                    const quantity = new BigNumber(
                      this.selectedAssetsAmounts[index] || 0
                    );
                    return {
                      policy_id: policyId,
                      asset_name: assetName,
                      quantity: quantity.toNumber(),
                    };
                  }
                );
                this.calculateTransactionFee(
                  receiverValue,
                  amountValue,
                  assets
                );
              } else {
                this.resetTransactionFee();
              }
              return [
                isValid,
                this.context.intl.formatMessage(messages.invalidAmount),
              ];
            },
          ],
        },
        estimatedFee: {
          label: this.context.intl.formatMessage(messages.estimatedFeeLabel),
          placeholder: `0${
            this.getCurrentNumberFormat().decimalSeparator
          }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
          value: null,
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnBlur: false,
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  setReceiverValidity(isValid: boolean) {
    if (this._isMounted) {
      this.setState({
        isReceiverAddressValid: isValid,
      });
    }
  }

  isLatestTransactionFeeRequest = (
    currentFeeCalculationRequestQue: number,
    prevFeeCalculationRequestQue: number
  ) => currentFeeCalculationRequestQue - prevFeeCalculationRequestQue === 1;

  calculateTransactionFee = async (
    address: string,
    amountValue: string,
    assets: AssetItems
  ) => {
    const amount = formattedAmountToLovelace(amountValue);
    const {
      feeCalculationRequestQue: prevFeeCalculationRequestQue,
    } = this.state;
    this.setState((prevState) => ({
      isTransactionFeeCalculated: false,
      transactionFee: new BigNumber(0),
      transactionFeeError: null,
      assetErrors: {},
      feeCalculationRequestQue: prevState.feeCalculationRequestQue + 1,
    }));
    try {
      this._isCalculatingTransactionFee = true;
      const { fee, minimumAda } = await this.props.calculateTransactionFee(
        address,
        amount,
        assets
      );
      if (
        this._isMounted &&
        this.isLatestTransactionFeeRequest(
          this.state.feeCalculationRequestQue,
          prevFeeCalculationRequestQue
        )
      ) {
        this._isCalculatingTransactionFee = false;
        this.setState({
          isTransactionFeeCalculated: true,
          transactionFee: fee,
          transactionFeeError: null,
          minimumAda,
        });
      }
    } catch (error) {
      if (
        this._isMounted &&
        this.isLatestTransactionFeeRequest(
          this.state.feeCalculationRequestQue,
          prevFeeCalculationRequestQue
        )
      ) {
        const errorHasLink = !!get(error, ['values', 'linkLabel']);
        let transactionFeeError;
        let localizableError = error;
        let values;

        if (error.id === 'api.errors.utxoTooSmall') {
          const minimumAda = get(error, 'values.minimumAda');
          if (minimumAda && !Number.isNaN(Number(minimumAda))) {
            localizableError = messages.minAdaRequiredTooltip;
            values = { minimumAda };
            this.setState({ minimumAda: new BigNumber(minimumAda) });
          }
        }

        if (errorHasLink) {
          transactionFeeError = (
            <FormattedHTMLMessageWithLink
              message={localizableError}
              onExternalLinkClick={this.props.onExternalLinkClick}
            />
          );
        } else {
          transactionFeeError = this.context.intl.formatMessage(
            localizableError,
            values
          );
        }

        this._isCalculatingTransactionFee = false;
        this.setState({
          isTransactionFeeCalculated: false,
          transactionFee: new BigNumber(0),
          transactionFeeError,
        });
      }
    }
  };

  resetTransactionFee() {
    if (this._isMounted) {
      this._isCalculatingTransactionFee = false;
      this.setState({
        isTransactionFeeCalculated: false,
        transactionFee: new BigNumber(0),
        transactionFeeError: null,
        assetErrors: {},
      });
    }
  }

  showRemoveAssetButton = (fingerprint: string) => {
    const { showRemoveAssetButton } = this.state;
    showRemoveAssetButton[fingerprint] = true;
    this.setState({
      showRemoveAssetButton,
    });
  };

  hideRemoveAssetButton = (fingerprint: string) => {
    const { showRemoveAssetButton } = this.state;
    showRemoveAssetButton[fingerprint] = false;
    this.setState({
      showRemoveAssetButton,
    });
  };

  addAssetRow = (fingerprint: string) => {
    this.addAssetFields(fingerprint);
    this.updateFormFields(false, fingerprint);
    const { selectedAssetFingerprints } = this.state;
    selectedAssetFingerprints.push(fingerprint);
    this.setState({
      selectedAssetFingerprints,
    });
  };

  removeAssetRow = (fingerprint: string) => {
    const { formFields, selectedAssetFingerprints } = this.state;
    const { receiver } = formFields;
    const assetFields = omit(receiver.assetFields, fingerprint);
    const assetsDropdown = omit(receiver.assetsDropdown, fingerprint);
    this.setState({
      selectedAssetFingerprints: without(
        selectedAssetFingerprints,
        fingerprint
      ),
      formFields: {
        ...formFields,
        receiver: {
          ...receiver,
          assetFields,
          assetsDropdown,
        },
      },
    });
    this.removeAssetFields(fingerprint);
  };

  addAssetFields = (fingerprint: string) => {
    const newAsset = `asset_${fingerprint}`;
    this.form.add({ name: newAsset, value: null, key: newAsset });
    this.form
      .$(newAsset)
      .set('label', this.context.intl.formatMessage(messages.assetLabel));
    this.form
      .$(newAsset)
      .set(
        'placeholder',
        `0${this.getCurrentNumberFormat().decimalSeparator}${'0'.repeat(
          this.props.currencyMaxFractionalDigits
        )}`
      );
    this.form.$(newAsset).set('validators', [
      async ({ field, form }) => {
        const amountValue = field.value ? field.value.toString() : '';
        const isValid = await this.props.validateAmount(
          formattedAmountToNaturalUnits(amountValue)
        );
        const receiverField = form.$('receiver');
        const receiverValue = receiverField.value;
        const isReceiverValid = receiverField.isValid;
        const adaAmountField = form.$('adaAmount');
        const adaAmountFieldValue = adaAmountField.value;
        const isAdaAmountValid = adaAmountField.isValid;

        const asset = this.getAssetByFingerprint(fingerprint);
        if (!asset) {
          return false;
        }
        const assetValue = new BigNumber(field.value);
        const assetMaxValue = new BigNumber(asset.quantity);
        const isAmountLessThanMax = assetValue.isLessThanOrEqualTo(
          assetMaxValue
        );

        if (
          isValid &&
          isAmountLessThanMax &&
          isReceiverValid &&
          isAdaAmountValid
        ) {
          this._isCalculatingTransactionFee = true;
          const assets = this.selectedAssets.map(
            ({ policyId, assetName }, index) => {
              const quantity = new BigNumber(
                this.selectedAssetsAmounts[index] || 0
              );
              return {
                policy_id: policyId,
                asset_name: assetName,
                quantity: quantity.toNumber(),
              };
            }
          );
          this.calculateTransactionFee(
            receiverValue,
            adaAmountFieldValue,
            assets
          );
        } else if (!isAmountLessThanMax) {
          const { assetErrors } = this.state;
          const error = this.context.intl.formatMessage(messages.invalidAmount);
          assetErrors[asset.fingerprint] = error;
          this._isCalculatingTransactionFee = false;
          this.setState({
            isTransactionFeeCalculated: false,
            transactionFee: new BigNumber(0),
            assetErrors,
          });
        } else {
          this.resetTransactionFee();
        }
        return [
          isValid,
          this.context.intl.formatMessage(messages.invalidAmount),
        ];
      },
    ]);
    this.form.$(newAsset).focus();

    const assetsDropdown = `assetsDropdown_${fingerprint}`;
    this.form.add({
      name: assetsDropdown,
      value: null,
      key: assetsDropdown,
    });
    this.form.$(assetsDropdown).set('type', 'select');
  };

  removeAssetFields = (fingerprint: string) => {
    const assetFieldToDelete = `asset_${fingerprint}`;
    this.form.del(assetFieldToDelete);
    const assetsDropdownFieldToDelete = `assetsDropdown_${fingerprint}`;
    this.form.del(assetsDropdownFieldToDelete);
  };

  onChangeAsset = (oldFingerprint: string, newFingerprint: string) => {
    this.addAssetFields(newFingerprint);
    this.updateFormFields(false, newFingerprint);

    let { selectedAssetFingerprints } = this.state;
    const index = findIndex(selectedAssetFingerprints, oldFingerprint);
    if (index > -1) {
      selectedAssetFingerprints = selectedAssetFingerprints.splice(
        index,
        1,
        newFingerprint
      );
    } else {
      selectedAssetFingerprints.push(newFingerprint);
    }
    this.setState({
      selectedAssetFingerprints,
    });

    this.removeAssetRow(oldFingerprint);
  };

  renderReceiverRow = (): Node => {
    const { intl } = this.context;
    const {
      formFields,
      minimumAda,
      transactionFee,
      transactionFeeError,
      assetErrors,
      selectedAssetFingerprints,
      isReceiverAddressValid,
      isTransactionFeeCalculated,
    } = this.state;
    const {
      currencyMaxFractionalDigits,
      walletAmount,
      isHardwareWallet,
      isClearTooltipOpeningDownward,
    } = this.props;

    const {
      receiver: receiverField,
      adaAmount,
      assetFields,
      assetsDropdown,
    } = formFields.receiver;

    const receiverLabel = intl.formatMessage(messages.receiverLabel);
    const adaAmountFieldProps = adaAmount.bind();

    let fees = null;
    if (isTransactionFeeCalculated && transactionFee) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
    }
    const estimatedFeeField = this.form.$('estimatedFee');

    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight = selectedAssetFingerprints.length
      ? assetsSeparatorBasicHeight * (selectedAssetFingerprints.length + 1) -
        40 * selectedAssetFingerprints.length
      : assetsSeparatorBasicHeight;

    const minimumAdaValue = minimumAda.isZero()
      ? TRANSACTION_MIN_ADA_VALUE
      : minimumAda.toFormat();

    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      !this.hasAvailableAssets ? styles.disabled : null,
      'primary',
    ]);

    return (
      <div className={styles.fieldsContainer}>
        <div className={styles.receiverInput}>
          <Input
            className="receiver"
            label={receiverLabel}
            {...receiverField.bind()}
            error={receiverField.error}
            onChange={(value) => {
              if (this._isMounted) {
                receiverField.onChange(value || '');
                this.setState({
                  isResetButtonDisabled: false,
                });
              }
            }}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {this.hasReceiverValue() && (
            <div className={styles.clearReceiverContainer}>
              <PopOver
                content={intl.formatMessage(messages.clearLabel)}
                placement={isClearTooltipOpeningDownward ? 'bottom' : 'top'}
              >
                <button
                  onClick={() => this.handleOnReset()}
                  className={styles.clearReceiverButton}
                >
                  <SVGInline
                    svg={closeIcon}
                    className={styles.clearReceiverIcon}
                  />
                </button>
              </PopOver>
            </div>
          )}
        </div>
        {this.hasReceiverValue() && isReceiverAddressValid && (
          <>
            <div
              className={styles.fieldsLine}
              style={{
                height: `${assetsSeparatorCalculatedHeight}px`,
                top: `${assetsSeparatorCalculatedHeight - 10}px`,
                marginTop: `-${assetsSeparatorCalculatedHeight}px`,
              }}
            />
            <div className={styles.assetInput}>
              <Fragment>
                {walletAmount && (
                  <div className={styles.amountTokenTotal}>
                    {intl.formatMessage(messages.ofLabel)}&nbsp;
                    {formattedWalletAmount(walletAmount)}
                  </div>
                )}
                <div className={styles.adaAmountLabel}>
                  {intl.formatMessage(globalMessages.unitAda)}
                </div>
                <NumericInput
                  {...adaAmountFieldProps}
                  className="adaAmount"
                  value={adaAmount.value}
                  label={`${intl.formatMessage(messages.assetAdaLabel)}`}
                  bigNumberFormat={this.getCurrentNumberFormat()}
                  decimalPlaces={currencyMaxFractionalDigits}
                  numberLocaleOptions={{
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  }}
                  onChange={(value) => {
                    this._isCalculatingTransactionFee = true;
                    adaAmount.onChange(value);
                    estimatedFeeField.onChange(fees);
                  }}
                  currency={globalMessages.unitAda}
                  error={adaAmount.error || transactionFeeError}
                  onKeyPress={this.handleSubmitOnEnter}
                  allowSigns={false}
                />
                <div className={styles.minAdaRequired}>
                  <span>
                    {intl.formatMessage(messages.minAdaRequired, {
                      minimumAda: minimumAdaValue,
                    })}
                  </span>
                  <PopOver
                    content={intl.formatMessage(
                      messages.minAdaRequiredTooltip,
                      {
                        minimumAda: minimumAdaValue,
                      }
                    )}
                    contentClassName={styles.minAdaTooltipContent}
                    key="tooltip"
                  >
                    <SVGInline
                      svg={questionMarkIcon}
                      className={styles.infoIcon}
                    />
                  </PopOver>
                </div>
              </Fragment>
              <Fragment>
                {selectedAssetFingerprints.map(
                  (fingerprint: string, index: number) => {
                    const asset = this.getAssetByFingerprint(fingerprint);
                    if (!asset) {
                      return false;
                    }
                    const { quantity, metadata } = asset;
                    const acronym = get(metadata, 'acronym', null);
                    const decimals = get(metadata, 'unit.decimals', 0);
                    const sortedAssets = orderBy(
                      [asset, ...this.availableAssets],
                      'fingerprint',
                      'asc'
                    );

                    const assetField = assetFields[fingerprint];
                    const assetFieldProps = assetField.bind();
                    const assetAmount = assetField.value
                      ? new BigNumber(assetField.value)
                      : '';

                    const assetsDropdownField = assetsDropdown[fingerprint];
                    const assetsDropdownFieldProps = assetsDropdownField.bind();

                    return (
                      <div
                        key={`receiver_asset_${fingerprint}`}
                        onMouseEnter={() =>
                          this.showRemoveAssetButton(fingerprint)
                        }
                        onMouseLeave={() =>
                          this.hideRemoveAssetButton(fingerprint)
                        }
                        className={styles.fieldContainer}
                      >
                        {quantity.isPositive() && (
                          <div className={styles.amountTokenTotal}>
                            {intl.formatMessage(messages.ofLabel)}&nbsp;
                            {formattedTokenWalletAmount(quantity, metadata)}
                          </div>
                        )}
                        <NumericInput
                          {...assetFieldProps}
                          placeholder={
                            decimals
                              ? `0${
                                  this.getCurrentNumberFormat().decimalSeparator
                                }${'0'.repeat(decimals)}`
                              : '0'
                          }
                          className={classNames([
                            styles.assetItem,
                            this.state.showRemoveAssetButton[fingerprint]
                              ? styles.hasButton
                              : null,
                          ])}
                          label={
                            <>
                              {`${intl.formatMessage(messages.assetLabel)} #${
                                index + 1
                              }`}
                              <Button
                                className={classNames([
                                  styles.removeAssetButton,
                                  'flat',
                                  this.state.showRemoveAssetButton[fingerprint]
                                    ? styles.active
                                    : null,
                                ])}
                                label={intl.formatMessage(
                                  messages.removeReceiverButtonLabel
                                )}
                                onClick={() => this.removeAssetRow(fingerprint)}
                              />
                            </>
                          }
                          bigNumberFormat={
                            decimals ? this.getCurrentNumberFormat() : null
                          }
                          decimalPlaces={decimals}
                          numberLocaleOptions={{
                            minimumFractionDigits: decimals,
                          }}
                          onChange={(value) => {
                            this._isCalculatingTransactionFee = true;
                            this.setState({
                              isResetButtonDisabled: false,
                            });
                            assetField.onChange(value);
                            estimatedFeeField.onChange(fees);
                          }}
                          currency={acronym}
                          value={assetAmount}
                          error={assetErrors[fingerprint]}
                          skin={AmountInputSkin}
                          onKeyPress={(
                            evt: SyntheticKeyboardEvent<EventTarget>
                          ) => {
                            const { charCode } = evt;
                            if (
                              charCode === 190 ||
                              charCode === 110 ||
                              charCode === 46
                            ) {
                              evt.persist();
                              evt.preventDefault();
                              evt.stopPropagation();
                            }
                            return this.handleSubmitOnEnter;
                          }}
                          allowSigns={false}
                        />
                        <div className={styles.rightContent}>
                          {this.hasAssetValue(assetFieldProps) && (
                            <div className={styles.clearAssetContainer}>
                              <PopOver
                                content={intl.formatMessage(
                                  messages.clearLabel
                                )}
                                placement={
                                  isClearTooltipOpeningDownward
                                    ? 'bottom'
                                    : 'top'
                                }
                              >
                                <button
                                  onClick={() =>
                                    this.clearAssetFieldValue(assetField)
                                  }
                                  className={styles.clearAssetButton}
                                >
                                  <SVGInline
                                    svg={closeIcon}
                                    className={styles.clearReceiverIcon}
                                  />
                                </button>
                              </PopOver>
                              <div className={styles.separator} />
                            </div>
                          )}
                          <div className={styles.assetsDropdownWrapper}>
                            <WalletsDropdown
                              className={styles.assetsDropdown}
                              {...assetsDropdownFieldProps}
                              assets={sortedAssets}
                              onChange={(newFingerprint) => {
                                if (newFingerprint !== fingerprint) {
                                  this.onChangeAsset(
                                    fingerprint,
                                    newFingerprint
                                  );
                                }
                              }}
                              syncingLabel={intl.formatMessage(
                                messages.syncingWallet
                              )}
                              hasAssetsEnabled
                              value={fingerprint}
                              getStakePoolById={() => {}}
                              errorPosition="bottom"
                            />
                          </div>
                        </div>
                      </div>
                    );
                  }
                )}
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                disabled={isHardwareWallet || !this.hasAvailableAssets}
                onClick={() => {
                  this.addAssetRow(this.availableAssets[0].fingerprint);
                }}
              />
            </div>
          </>
        )}
      </div>
    );
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      formFields,
      transactionFee,
      transactionFeeError,
      isResetButtonDisabled,
      isTransactionFeeCalculated,
    } = this.state;
    const {
      currencyUnit,
      currencyMaxFractionalDigits,
      hwDeviceStatus,
      isHardwareWallet,
      isDialogOpen,
      isRestoreActive,
      onExternalLinkClick,
    } = this.props;

    const receiverField = form.$('receiver');
    const receiverFieldProps = receiverField.bind();
    const receiver = receiverFieldProps.value;

    const adaAmountField = form.$('adaAmount');
    const adaAmountFieldProps = adaAmountField.bind();
    const adaAmount = new BigNumber(adaAmountFieldProps.value || 0);

    let fees = null;
    let total = null;
    if (isTransactionFeeCalculated) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = adaAmount
        .plus(transactionFee)
        .toFormat(currencyMaxFractionalDigits);
    }

    const calculatingFeesSpinnerButtonClasses = classNames([
      styles.calculatingFeesSpinnerButton,
      styles.spinning,
    ]);

    return (
      <div className={styles.component}>
        {isRestoreActive ? (
          <div className={styles.syncingTransactionsWrapper}>
            <LoadingSpinner big />
            <p className={styles.syncingTransactionsText}>
              {intl.formatMessage(messages.syncingTransactionsMessage)}
            </p>
          </div>
        ) : (
          <BorderedBox>
            <div className={styles.walletAssetsSendForm}>
              {formFields.receiver && this.renderReceiverRow()}
              <div className={styles.estimatedFeeInput}>
                <ReadOnlyInput
                  label={intl.formatMessage(messages.estimatedFeeLabel)}
                  value={
                    fees && !transactionFeeError
                      ? `${fees} ${intl.formatMessage(globalMessages.unitAda)}`
                      : `0${
                          this.getCurrentNumberFormat().decimalSeparator
                        }${'0'.repeat(
                          this.props.currencyMaxFractionalDigits
                        )} ${intl.formatMessage(globalMessages.unitAda)}`
                  }
                  isSet
                />
                {this._isCalculatingTransactionFee && (
                  <div className={styles.calculatingFeesContainer}>
                    <PopOver
                      content={intl.formatMessage(
                        messages.calculatingFeesLabel
                      )}
                    >
                      <button className={calculatingFeesSpinnerButtonClasses} />
                    </PopOver>
                  </div>
                )}
              </div>
              <div className={styles.buttonsContainer}>
                <Button
                  className="flat"
                  label={intl.formatMessage(messages.resetButtonLabel)}
                  disabled={isResetButtonDisabled}
                  onClick={this.handleOnReset}
                />
                <Button
                  className="primary"
                  label={intl.formatMessage(messages.sendButtonLabel)}
                  disabled={this.isDisabled()}
                  onClick={this.handleOnSubmit}
                />
              </div>
            </div>
          </BorderedBox>
        )}

        {isDialogOpen(WalletAssetsSendConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
            currencyUnit={currencyUnit}
            receiver={receiver}
            assets={this.selectedAssets}
            assetsAmounts={this.selectedAssetsAmounts}
            amount={adaAmount.toFormat(currencyMaxFractionalDigits)}
            amountToNaturalUnits={formattedAmountToNaturalUnits}
            totalAmount={total}
            transactionFee={fees}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onExternalLinkClick={onExternalLinkClick}
          />
        ) : null}
      </div>
    );
  }
}

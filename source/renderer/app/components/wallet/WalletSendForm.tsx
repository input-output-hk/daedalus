import React, { Component, Fragment } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import type { Field } from 'mobx-react-form';
import { observer } from 'mobx-react';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import { filter, get, indexOf, omit, map, without } from 'lodash';
import BigNumber from 'bignumber.js';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import BorderedBox from '../widgets/BorderedBox';
import LoadingSpinner from '../widgets/LoadingSpinner';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import { FormattedHTMLMessageWithLink } from '../widgets/FormattedHTMLMessageWithLink';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/question-m... Remove this comment to see the full error message
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../assets/images/close-cros... Remove this comment to see the full error message
import closeIcon from '../../assets/images/close-cross.inline.svg';
import globalMessages from '../../i18n/global-messages';
import messages from './send-form/messages';
import { messages as apiErrorMessages } from '../../api/errors';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import {
  formattedAmountToNaturalUnits,
  formattedAmountToLovelace,
} from '../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { TRANSACTION_MIN_ADA_VALUE } from '../../config/walletsConfig';
import { NUMBER_FORMATS } from '../../../../common/types/number.types';
import AssetInput from './send-form/AssetInput';
import WalletSendAssetsConfirmationDialog from './send-form/WalletSendAssetsConfirmationDialog';
import WalletSendConfirmationDialogContainer from '../../containers/wallet/dialogs/WalletSendConfirmationDialogContainer';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSendForm.scss' or its ... Remove this comment to see the full error message
import styles from './WalletSendForm.scss';
import Asset from '../../domains/Asset';
import type { HwDeviceStatus } from '../../domains/Wallet';
import type { AssetToken, ApiTokens } from '../../api/assets/types';
import { DiscreetWalletAmount } from '../../features/discreet-mode';

messages.fieldIsRequired = globalMessages.fieldIsRequired;
type Props = {
  currencyMaxIntegerDigits: number;
  currencyMaxFractionalDigits: number;
  currentNumberFormat: string;
  calculateTransactionFee: (...args: Array<any>) => any;
  walletAmount: BigNumber;
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>;
  validateAssetAmount: (amountInNaturalUnits: string) => Promise<boolean>;
  addressValidator: (...args: Array<any>) => any;
  assets: Array<AssetToken>;
  hasAssets: boolean;
  selectedAsset: Asset | null | undefined;
  isLoadingAssets: boolean;
  isDialogOpen: (...args: Array<any>) => any;
  isRestoreActive: boolean;
  isHardwareWallet: boolean;
  hwDeviceStatus: HwDeviceStatus;
  onOpenDialogAction: (...args: Array<any>) => any;
  onUnsetActiveAsset: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
  isAddressFromSameWallet: boolean;
};
type State = {
  formFields: {
    receiver: {
      receiver: Field;
      adaAmount: Field;
      assetFields: Record<string, Field>;
      assetsDropdown: Record<string, Field>;
    };
  };
  minimumAda: BigNumber;
  feeCalculationRequestQue: number;
  transactionFee: BigNumber;
  transactionFeeError: (string | null | undefined) | (Node | null | undefined);
  showRemoveAssetButton: Record<string, boolean>;
  selectedAssetUniqueIds: Array<string>;
  isResetButtonDisabled: boolean;
  isReceiverAddressValid: boolean;
  isTransactionFeeCalculated: boolean;
};

@observer
class WalletSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  // @ts-ignore ts-migrate(2416) FIXME: Property 'state' in type 'WalletSendForm' is not a... Remove this comment to see the full error message
  state = {
    formFields: {},
    minimumAda: new BigNumber(0),
    feeCalculationRequestQue: 0,
    transactionFee: new BigNumber(0),
    transactionFeeError: null,
    showRemoveAssetButton: {},
    selectedAssetUniqueIds: [],
    isResetButtonDisabled: true,
    isReceiverAddressValid: false,
    isTransactionFeeCalculated: false,
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
  // We need to prevent auto focus of ada and token amount fields in case user pastes
  // or enters a receiver address which belongs to the same wallet he is sending from.
  _isAutoFocusEnabled = true;

  componentDidMount() {
    this._isMounted = true;
    this.updateFormFields(true);
    const { selectedAsset } = this.props;

    if (selectedAsset) {
      setTimeout(() => {
        if (this._isMounted) {
          this.addAssetRow(selectedAsset.uniqueId);
        }
      });
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.props.onUnsetActiveAsset();
  }

  getCurrentNumberFormat() {
    return NUMBER_FORMATS[this.props.currentNumberFormat];
  }

  get selectedAssets(): Array<AssetToken> {
    const { selectedAssetUniqueIds } = this.state;
    const { assets: allAssets } = this.props;
    return map(selectedAssetUniqueIds, (uniqueId) =>
      allAssets.find((asset) => asset.uniqueId === uniqueId)
    );
  }

  get selectedAssetsAmounts(): Array<string> {
    const { selectedAssetUniqueIds, formFields } = this.state;
    const assetFields = get(formFields, 'receiver.assetFields');
    return map(selectedAssetUniqueIds, (uniqueId) =>
      formattedAmountToNaturalUnits(assetFields[uniqueId].value)
    );
  }

  get availableAssets(): Array<AssetToken> {
    const { assets: allAssets } = this.props;
    const { selectedAssetUniqueIds } = this.state;
    return filter(
      allAssets,
      ({ uniqueId }) => !selectedAssetUniqueIds.includes(uniqueId)
    );
  }

  get hasAvailableAssets(): boolean {
    return this.availableAssets.length > 0;
  }

  getAssetByUniqueId = (uniqueId: string): AssetToken | null | undefined => {
    const { assets: allAssets } = this.props;
    return allAssets.find((asset) => asset.uniqueId === uniqueId);
  };
  focusableFields: Record<string, Field> = {};
  addFocusableField = (field: Field | null | undefined) => {
    if (field) {
      const { name: fieldName } = field.props;
      this.focusableFields[fieldName] = field;
    }
  };
  focusField = (field: Field) => {
    const { name: fieldName } = field;
    const focusableField = this.focusableFields[fieldName];

    if (focusableField) {
      focusableField.focus();
    }
  };
  handleSubmitOnEnter = (event: KeyboardEvent): void => {
    if (event.target instanceof HTMLInputElement && event.key === 'Enter')
      this.handleOnSubmit();
  };
  handleOnSubmit = () => {
    if (this.isDisabled()) {
      return;
    }

    this.props.onOpenDialogAction({
      dialog: WalletSendAssetsConfirmationDialog,
    });
  };
  handleOnReset = () => {
    // Cancel all debounced field validations
    // @ts-ignore ts-migrate(2339) FIXME: Property 'each' does not exist on type 'ReactToolb... Remove this comment to see the full error message
    this.form.each((field) => {
      field.debouncedValidation.cancel();
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'reset' does not exist on type 'ReactTool... Remove this comment to see the full error message
    this.form.reset();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'showErrors' does not exist on type 'Reac... Remove this comment to see the full error message
    this.form.showErrors(false);
    this.clearReceiverFieldValue();
    this.clearAdaAmountFieldValue();
    this.updateFormFields(true);
    this.setState({
      minimumAda: new BigNumber(0),
      showRemoveAssetButton: {},
      isResetButtonDisabled: true,
    });
  };
  clearReceiverFieldValue = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const receiverField = this.form.$('receiver');

    if (receiverField) {
      receiverField.clear();
      this.setReceiverValidity(false);
      this.focusField(receiverField);
    }
  };
  clearAdaAmountFieldValue = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const adaAmountField = this.form.$('adaAmount');

    if (adaAmountField) {
      adaAmountField.clear();
    }
  };
  clearAssetFieldValue = (assetField: Field) => {
    if (assetField) {
      assetField.clear();
      this.focusField(assetField);
    }

    this.resetTransactionFee();
  };
  updateFormFields = (resetFormFields: boolean, uniqueId?: string) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'fields' does not exist on type 'ReactToo... Remove this comment to see the full error message
    const formFields = this.form.fields;
    const receiverField = formFields.get('receiver');
    const adaAmountField = formFields.get('adaAmount');

    if (resetFormFields) {
      this.setState({
        selectedAssetUniqueIds: [],
        formFields: {
          receiver: {
            receiver: receiverField,
            adaAmount: adaAmountField,
            assetFields: {},
            assetsDropdown: {},
          },
        },
      });
    } else if (uniqueId) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'receiver' does not exist on type '{}'.
      const { assetFields, assetsDropdown } = this.state.formFields.receiver;
      const assetField = formFields.get(`asset_${uniqueId}`);

      if (assetField) {
        assetFields[uniqueId] = assetField;
      }

      const assetsDropdownField = formFields.get(`assetsDropdown_${uniqueId}`);

      if (assetsDropdownField) {
        assetsDropdown[uniqueId] = assetsDropdownField;
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
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const receiverField = this.form.$('receiver');
    return receiverField.value.length > 0;
  };
  isAddressFromSameWallet = () => {
    const { isAddressFromSameWallet } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const receiverField = this.form.$('receiver');
    return (
      this.hasReceiverValue() &&
      isAddressFromSameWallet &&
      receiverField.isValid
    );
  };
  isDisabled = () =>
    this._isCalculatingTransactionFee ||
    !this.state.isTransactionFeeCalculated ||
    // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
    !this.form.isValid;
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        receiver: {
          label: this.context.intl.formatMessage(messages.receiverLabel),
          placeholder: this.context.intl.formatMessage(messages.receiverHint),
          value: '',
          validators: [
            async ({ field, form }) => {
              const { value } = field;

              if (value === null || value === '') {
                this.resetTransactionFee();
                this.setReceiverValidity(false);
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }

              const isValid = await this.props.addressValidator(value);

              if (isValid && this.isAddressFromSameWallet()) {
                this._isAutoFocusEnabled = false;
              }

              this.setReceiverValidity(isValid);
              const adaAmountField = form.$('adaAmount');
              const isAdaAmountValid = adaAmountField.isValid;

              if (isValid && isAdaAmountValid) {
                this.calculateTransactionFee();
              } else {
                this.resetTransactionFee();
              }

              return [
                isValid,
                this.context.intl.formatMessage(
                  apiErrorMessages.invalidAddress
                ),
              ];
            },
          ],
        },
        adaAmount: {
          label: this.context.intl.formatMessage(messages.adaAmountLabel),
          placeholder: `0${
            this.getCurrentNumberFormat().decimalSeparator
          }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
          value: '',
          validators: [
            async ({ field }) => {
              const { value } = field;

              if (value === null || value === '') {
                this.resetTransactionFee();
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }

              const amountValue = value.toString();
              const isValid = await this.props.validateAmount(
                formattedAmountToNaturalUnits(amountValue)
              );

              if (isValid) {
                this.calculateTransactionFee();
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
      plugins: {
        vjf: vjf(),
      },
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
  calculateTransactionFee = async () => {
    const { form } = this;
    const emptyAssetFieldValue = '0';
    const hasEmptyAssetFields = this.selectedAssetsAmounts.includes(
      emptyAssetFieldValue
    );

    // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
    if (!form.isValid || hasEmptyAssetFields) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'showErrors' does not exist on type 'Reac... Remove this comment to see the full error message
      form.showErrors(true);
      return;
    }

    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const receiverField = form.$('receiver');
    const receiver = receiverField.value;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const adaAmountField = form.$('adaAmount');
    const adaAmount = formattedAmountToLovelace(adaAmountField.value);
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ policy_id: string; asset_name: string; qua... Remove this comment to see the full error message
    const assets: ApiTokens = filter(
      this.selectedAssets.map(({ policyId, assetName }, index) => {
        const quantity = new BigNumber(this.selectedAssetsAmounts[index]);
        return {
          policy_id: policyId,
          asset_name: assetName,
          quantity, // BigNumber or number - prevent parsing a BigNumber to Number (Integer) because of JS number length limitation
        };
      }),
      'quantity'
    );
    const {
      selectedAssetUniqueIds,
      feeCalculationRequestQue: prevFeeCalculationRequestQue,
    } = this.state;
    this.setState((prevState) => ({
      feeCalculationRequestQue: prevState.feeCalculationRequestQue + 1,
      isTransactionFeeCalculated: false,
      transactionFee: new BigNumber(0),
      transactionFeeError: null,
    }));

    try {
      this._isCalculatingTransactionFee = true;
      const { fee, minimumAda } = await this.props.calculateTransactionFee(
        receiver,
        adaAmount,
        assets
      );

      if (
        this._isMounted &&
        this.isLatestTransactionFeeRequest(
          this.state.feeCalculationRequestQue,
          prevFeeCalculationRequestQue
        ) &&
        !this.selectedAssetsAmounts.includes(emptyAssetFieldValue)
      ) {
        this._isCalculatingTransactionFee = false;
        this.setState({
          isTransactionFeeCalculated: true,
          minimumAda: minimumAda || new BigNumber(0),
          transactionFee: fee,
          transactionFeeError: null,
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
            localizableError = selectedAssetUniqueIds.length
              ? messages.minAdaRequiredWithAssetTooltip
              : messages.minAdaRequiredWithNoAssetTooltip;
            values = {
              minimumAda,
            };
            this.setState({
              minimumAda: new BigNumber(minimumAda),
            });
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
          transactionFeeError = (
            <FormattedHTMLMessage {...localizableError} values={values} />
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
      });
    }
  }

  showRemoveAssetButton = (uniqueId: string) => {
    const { showRemoveAssetButton } = this.state;
    showRemoveAssetButton[uniqueId] = true;
    this.setState({
      showRemoveAssetButton,
    });
  };
  hideRemoveAssetButton = (uniqueId: string) => {
    const { showRemoveAssetButton } = this.state;
    showRemoveAssetButton[uniqueId] = false;
    this.setState({
      showRemoveAssetButton,
    });
  };
  addAssetRow = (uniqueId: string) => {
    this.addAssetFields(uniqueId);
    this.updateFormFields(false, uniqueId);
    const { selectedAssetUniqueIds } = this.state;
    selectedAssetUniqueIds.push(uniqueId);
    this.setState({
      selectedAssetUniqueIds,
    });
    this.resetTransactionFee();
    this._isAutoFocusEnabled = true;
  };
  removeAssetRow = (uniqueId: string) => {
    const { formFields, selectedAssetUniqueIds } = this.state;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'receiver' does not exist on type '{}'.
    const { receiver } = formFields;
    const assetFields = omit(receiver.assetFields, uniqueId);
    const assetsDropdown = omit(receiver.assetsDropdown, uniqueId);
    this.setState({
      selectedAssetUniqueIds: without(selectedAssetUniqueIds, uniqueId),
      formFields: {
        ...formFields,
        receiver: { ...receiver, assetFields, assetsDropdown },
      },
    });
    this.removeAssetFields(uniqueId);
    setTimeout(() => {
      this.calculateTransactionFee();
    });
  };
  addAssetFields = (uniqueId: string) => {
    const newAsset = `asset_${uniqueId}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'add' does not exist on type 'ReactToolbo... Remove this comment to see the full error message
    this.form.add({
      name: newAsset,
      value: null,
      key: newAsset,
    });
    this.form
      // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
      .$(newAsset)
      .set('label', this.context.intl.formatMessage(messages.assetLabel));
    this.form
      // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
      .$(newAsset)
      .set(
        'placeholder',
        `0${this.getCurrentNumberFormat().decimalSeparator}${'0'.repeat(
          this.props.currencyMaxFractionalDigits
        )}`
      );
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    this.form.$(newAsset).set('validators', [
      async ({ field }) => {
        const { value } = field;

        if (value === null || value === '') {
          this.resetTransactionFee();
          return [
            false,
            this.context.intl.formatMessage(messages.fieldIsRequired),
          ];
        }

        const amountValue = value.toString();
        const isValidAmount = await this.props.validateAssetAmount(
          formattedAmountToNaturalUnits(amountValue)
        );
        const asset = this.getAssetByUniqueId(uniqueId);

        if (!asset) {
          return false;
        }

        const assetValue = new BigNumber(
          formattedAmountToNaturalUnits(field.value)
        );
        const isValidRange =
          assetValue.isGreaterThan(0) &&
          assetValue.isLessThanOrEqualTo(asset.quantity);
        const isValid = isValidAmount && isValidRange;

        if (isValid) {
          this.calculateTransactionFee();
        } else {
          this.resetTransactionFee();
        }

        return [
          isValid,
          this.context.intl.formatMessage(messages.invalidAmount),
        ];
      },
    ]);
    const assetsDropdown = `assetsDropdown_${uniqueId}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'add' does not exist on type 'ReactToolbo... Remove this comment to see the full error message
    this.form.add({
      name: assetsDropdown,
      value: null,
      key: assetsDropdown,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    this.form.$(assetsDropdown).set('type', 'select');
  };
  removeAssetFields = (uniqueId: string) => {
    const assetFieldToDelete = `asset_${uniqueId}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'del' does not exist on type 'ReactToolbo... Remove this comment to see the full error message
    this.form.del(assetFieldToDelete);
    const assetsDropdownFieldToDelete = `assetsDropdown_${uniqueId}`;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'del' does not exist on type 'ReactToolbo... Remove this comment to see the full error message
    this.form.del(assetsDropdownFieldToDelete);
  };
  onChangeAsset = async (currentUniqueId: string, newUniqueId: string) => {
    if (currentUniqueId === newUniqueId) return;
    this.addAssetFields(newUniqueId);
    this.updateFormFields(false, newUniqueId);
    const { selectedAssetUniqueIds: oldSelectedAssetUniqueIds } = this.state;
    const selectedAssetUniqueIds = [...oldSelectedAssetUniqueIds];
    const index = indexOf(selectedAssetUniqueIds, currentUniqueId);

    if (index > -1) {
      selectedAssetUniqueIds.splice(index, 1, newUniqueId);
    } else {
      selectedAssetUniqueIds.push(newUniqueId);
    }

    await this.setState({
      selectedAssetUniqueIds,
    });
    this.removeAssetRow(currentUniqueId);
    this.resetTransactionFee();
  };
  renderReceiverRow = (): Node => {
    const { intl } = this.context;
    const {
      formFields,
      minimumAda,
      transactionFeeError,
      selectedAssetUniqueIds,
      isReceiverAddressValid,
    } = this.state;
    const { currencyMaxFractionalDigits, walletAmount } = this.props;
    const {
      adaAmount: adaAmountField,
      receiver: receiverField,
      assetFields,
      assetsDropdown,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'receiver' does not exist on type '{}'.
    } = formFields.receiver;
    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight = selectedAssetUniqueIds.length
      ? assetsSeparatorBasicHeight * (selectedAssetUniqueIds.length + 1) -
        40 * selectedAssetUniqueIds.length
      : assetsSeparatorBasicHeight;
    const minimumAdaValue = minimumAda.isZero()
      ? TRANSACTION_MIN_ADA_VALUE
      : minimumAda.toFormat();
    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      !this.hasAvailableAssets ? styles.disabled : null,
      'primary',
    ]);
    const receiverFieldClasses = classNames([
      styles.receiverInput,
      this.isAddressFromSameWallet() ? styles.sameReceiverInput : null,
    ]);
    const minAdaRequiredTooltip = selectedAssetUniqueIds.length
      ? messages.minAdaRequiredWithAssetTooltip
      : messages.minAdaRequiredWithNoAssetTooltip;
    const sameWalletError = intl.formatMessage(messages.sameWalletLabel);
    let receiverFieldError = receiverField.error;
    let receiverFieldThemeVars = {};

    if (this.isAddressFromSameWallet()) {
      receiverFieldError = sameWalletError;
      receiverFieldThemeVars = {
        '--rp-input-border-color-errored':
          'var(--rp-password-input-warning-score-color)',
        '--rp-pop-over-bg-color':
          'var(--rp-password-input-warning-score-color)',
      };
    }

    return (
      <div className={styles.fieldsContainer}>
        <div className={receiverFieldClasses}>
          <Input
            {...receiverField.bind()}
            ref={(field) => {
              this.addFocusableField(field);
            }}
            className="receiver"
            error={receiverFieldError}
            onChange={(value) => {
              receiverField.onChange(value || '');
              this.setState({
                isResetButtonDisabled: false,
              });
            }}
            onKeyPress={this.handleSubmitOnEnter}
            themeVariables={receiverFieldThemeVars}
          />
          {this.hasReceiverValue() && (
            <div className={styles.clearReceiverContainer}>
              <PopOver
                content={intl.formatMessage(messages.clearLabel)}
                placement="top"
              >
                <button
                  onClick={() => this.handleOnReset()}
                  className={styles.clearReceiverButton}
                  tabIndex={-1}
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
                    {intl.formatMessage(messages.ofLabel)}{' '}
                    <DiscreetWalletAmount amount={walletAmount} />
                  </div>
                )}
                <div className={styles.adaAmountLabel}>
                  {intl.formatMessage(globalMessages.adaUnit)}
                </div>
                <NumericInput
                  {...adaAmountField.bind()}
                  ref={(field) => {
                    this.addFocusableField(field);
                  }}
                  className="adaAmount"
                  value={adaAmountField.value}
                  bigNumberFormat={this.getCurrentNumberFormat()}
                  decimalPlaces={currencyMaxFractionalDigits}
                  numberLocaleOptions={{
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  }}
                  onChange={(value) => {
                    adaAmountField.onChange(value);
                  }}
                  currency={globalMessages.adaUnit}
                  error={adaAmountField.error || transactionFeeError}
                  onKeyPress={this.handleSubmitOnEnter}
                  allowSigns={false}
                  autoFocus={this._isAutoFocusEnabled}
                />
                <div className={styles.minAdaRequired}>
                  <span>
                    {intl.formatMessage(messages.minAdaRequired, {
                      minimumAda: minimumAdaValue,
                    })}
                  </span>
                  <PopOver
                    content={intl.formatMessage(minAdaRequiredTooltip, {
                      minimumAda: minimumAdaValue,
                    })}
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
                {selectedAssetUniqueIds.map(
                  (uniqueId: string, index: number) => (
                    <AssetInput
                      key={uniqueId}
                      uniqueId={uniqueId}
                      index={index}
                      getAssetByUniqueId={this.getAssetByUniqueId}
                      availableAssets={this.availableAssets}
                      assetFields={assetFields}
                      assetsDropdown={assetsDropdown}
                      addFocusableField={this.addFocusableField}
                      removeAssetButtonVisible={
                        this.state.showRemoveAssetButton
                      }
                      showRemoveAssetButton={this.showRemoveAssetButton}
                      hideRemoveAssetButton={this.hideRemoveAssetButton}
                      currentNumberFormat={this.getCurrentNumberFormat()}
                      removeAssetRow={this.removeAssetRow}
                      handleSubmitOnEnter={this.handleSubmitOnEnter}
                      clearAssetFieldValue={this.clearAssetFieldValue}
                      onChangeAsset={(newUniqueId) =>
                        this.onChangeAsset(uniqueId, newUniqueId)
                      }
                      autoFocus={this._isAutoFocusEnabled}
                    />
                  )
                )}
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                disabled={!this.hasAvailableAssets}
                onClick={() => {
                  this.addAssetRow(this.availableAssets[0].uniqueId);
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
      currencyMaxFractionalDigits,
      hwDeviceStatus,
      isHardwareWallet,
      isDialogOpen,
      isRestoreActive,
      onExternalLinkClick,
    } = this.props;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const receiverField = form.$('receiver');
    const receiver = receiverField.value;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const adaAmountField = form.$('adaAmount');
    const adaAmount = new BigNumber(adaAmountField.value || 0);
    let fees = '0';
    let total: BigNumber = adaAmount;

    if (isTransactionFeeCalculated) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = adaAmount.plus(transactionFee);
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
            <div className={styles.walletSendForm}>
              {/* @ts-ignore ts-migrate(2339) FIXME: Property 'receiver' does not exist on type '{}'. */}
              {formFields.receiver && this.renderReceiverRow()}
              <div className={styles.estimatedFeeInput}>
                <ReadOnlyInput
                  label={intl.formatMessage(messages.estimatedFeeLabel)}
                  value={
                    fees && !transactionFeeError
                      ? `${fees} ${intl.formatMessage(globalMessages.adaUnit)}`
                      : `0${
                          this.getCurrentNumberFormat().decimalSeparator
                        }${'0'.repeat(
                          this.props.currencyMaxFractionalDigits
                        )} ${intl.formatMessage(globalMessages.adaUnit)}`
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

        {isDialogOpen(WalletSendAssetsConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
            receiver={receiver}
            selectedAssets={this.selectedAssets}
            assetsAmounts={this.selectedAssetsAmounts}
            amount={adaAmount.toFormat(currencyMaxFractionalDigits)}
            amountToNaturalUnits={formattedAmountToNaturalUnits}
            totalAmount={total}
            transactionFee={fees}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onExternalLinkClick={onExternalLinkClick}
            formattedTotalAmount={total.toFormat(currencyMaxFractionalDigits)}
          />
        ) : null}
      </div>
    );
  }
}

export default WalletSendForm;

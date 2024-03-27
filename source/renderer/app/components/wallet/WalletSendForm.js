'use strict';
// @ts-nocheck
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const lodash_1 = require('lodash');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const Button_1 = require('@react-polymorph/components/Button');
const Input_1 = require('@react-polymorph/components/Input');
const NumericInput_1 = require('@react-polymorph/components/NumericInput');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const BorderedBox_1 = __importDefault(require('../widgets/BorderedBox'));
const LoadingSpinner_1 = __importDefault(require('../widgets/LoadingSpinner'));
const ReadOnlyInput_1 = __importDefault(
  require('../widgets/forms/ReadOnlyInput')
);
const FormattedHTMLMessageWithLink_1 = require('../widgets/FormattedHTMLMessageWithLink');
const question_mark_inline_svg_1 = __importDefault(
  require('../../assets/images/question-mark.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../i18n/global-messages')
);
const messages_1 = __importDefault(require('./send-form/messages'));
const errors_1 = require('../../api/errors');
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../utils/ReactToolboxMobxForm')
);
const formatters_1 = require('../../utils/formatters');
const timingConfig_1 = require('../../config/timingConfig');
const walletsConfig_1 = require('../../config/walletsConfig');
const number_types_1 = require('../../../../common/types/number.types');
const AssetInput_1 = __importDefault(require('./send-form/AssetInput'));
const SendConfirmation_view_1 = require('../../containers/wallet/dialogs/send-confirmation/SendConfirmation.view');
const SendConfirmation_container_1 = require('../../containers/wallet/dialogs/send-confirmation/SendConfirmation.container');
const WalletSendForm_scss_1 = __importDefault(require('./WalletSendForm.scss'));
const discreet_mode_1 = require('../../features/discreet-mode');
const WalletTokenPicker_1 = __importDefault(
  require('./tokens/wallet-token-picker/WalletTokenPicker')
);
const ClearButton_1 = require('./widgets/ClearButton');
const VerticalSeparator_1 = require('./widgets/VerticalSeparator');
const analytics_1 = require('../../analytics');
messages_1.default.fieldIsRequired = global_messages_1.default.fieldIsRequired;
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
const AdaInputStateType = {
  Restored: 'restored',
  Updated: 'updated',
  None: 'none',
  Reset: 'reset',
};
let WalletSendForm = class WalletSendForm extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
  };
  state = {
    formFields: {},
    minimumAda: new bignumber_js_1.default(0),
    adaAmountInputTrack: new bignumber_js_1.default(0),
    transactionFee: new bignumber_js_1.default(0),
    transactionFeeError: null,
    selectedAssetUniqueIds: [],
    isResetButtonDisabled: true,
    isReceiverAddressValid: false,
    isReceiverAddressValidOnce: false,
    isCalculatingTransactionFee: false,
    adaInputState: AdaInputStateType.None,
    hasPendingRequestTokens: false,
    coinSelection: null,
    adaAmount: 0,
  };
  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;
  // We need to prevent auto focus of ada and token amount fields in case user pastes
  // or enters a receiver address which belongs to the same wallet he is sending from.
  _isAutoFocusEnabled = true;
  requestTokens = [];
  form;
  constructor(props, context) {
    super(props, context);
    this.form = this.setupMobxForm();
  }
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
    return number_types_1.NUMBER_FORMATS[this.props.currentNumberFormat];
  }
  get selectedAssets() {
    const { selectedAssetUniqueIds } = this.state;
    const { assets: allAssets } = this.props;
    return (0, lodash_1.map)(selectedAssetUniqueIds, (uniqueId) =>
      allAssets.find((asset) => asset.uniqueId === uniqueId)
    );
  }
  get selectedAssetsAmounts() {
    const { selectedAssetUniqueIds, formFields } = this.state;
    const assetFields = (0, lodash_1.get)(formFields, 'receiver.assetFields');
    return (0, lodash_1.map)(selectedAssetUniqueIds, (uniqueId) =>
      (0, formatters_1.formattedAmountToNaturalUnits)(
        assetFields[uniqueId].value
      )
    );
  }
  get availableAssets() {
    const { assets: allAssets } = this.props;
    const { selectedAssetUniqueIds } = this.state;
    return (0, lodash_1.filter)(
      allAssets,
      ({ uniqueId }) => !selectedAssetUniqueIds.includes(uniqueId)
    );
  }
  get hasAvailableAssets() {
    return this.availableAssets.length > 0;
  }
  getAssetByUniqueId = (uniqueId) => {
    const { assets: allAssets } = this.props;
    return allAssets.find((asset) => asset.uniqueId === uniqueId);
  };
  focusableFields = {};
  addFocusableField = (field) => {
    if (field) {
      const { name: fieldName } = field.props;
      this.focusableFields[fieldName] = field;
    }
  };
  focusField = (field) => {
    const { name: fieldName } = field;
    const focusableField = this.focusableFields[fieldName];
    if (focusableField) {
      focusableField.focus();
    }
  };
  handleSubmitOnEnter = (event) => {
    if (event.target instanceof HTMLInputElement && event.key === 'Enter') {
      this.handleOnSubmit();
    }
  };
  handleOnSubmit = async () => {
    if (this.isDisabled()) {
      return;
    }
    await this.waitForInFlightValidations();
    const adaAmountField = this.form.$('adaAmount');
    const adaAmount = new bignumber_js_1.default(adaAmountField.value || 0);
    this.props.onSubmit({
      coinSelection: this.state.coinSelection,
      receiver: this.form.$('receiver').value,
      amount: adaAmount,
      assetsAmounts: this.selectedAssetsAmounts,
      selectedAssets: this.selectedAssets,
      totalAmount: adaAmount.plus(this.state.transactionFee),
      transactionFee: this.state.transactionFee,
      adaAmount: this.state.adaAmount,
    });
  };
  waitForInFlightValidations = () =>
    new Promise((resolve) =>
      setTimeout(resolve, this.props.validationDebounceWait)
    );
  handleOnReset = () => {
    // Cancel all debounced field validations
    this.form.each((field) => {
      field.debouncedValidation.cancel();
    });
    this.form.reset();
    this.form.showErrors(false);
    this.clearReceiverFieldValue();
    this.clearAdaAmountFieldValue();
    this.updateFormFields(true);
    this.setState({
      minimumAda: new bignumber_js_1.default(0),
      adaAmountInputTrack: new bignumber_js_1.default(0),
      isResetButtonDisabled: true,
      adaInputState: AdaInputStateType.None,
      isReceiverAddressValidOnce: false,
    });
  };
  clearReceiverFieldValue = () => {
    const receiverField = this.form.$('receiver');
    if (receiverField) {
      receiverField.onChange('');
      this.setReceiverValidity(false);
      this.focusField(receiverField);
    }
  };
  clearAdaAmountFieldValue = () => {
    const adaAmountField = this.form.$('adaAmount');
    if (adaAmountField) {
      adaAmountField.clear();
    }
  };
  clearAssetFieldValue = (assetField) => {
    if (assetField) {
      assetField.clear();
      this.focusField(assetField);
    }
    this.resetTransactionFee();
  };
  updateFormFields = (resetFormFields, uniqueId) => {
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
    const receiverField = this.form.$('receiver');
    return receiverField.value.length > 0;
  };
  hasAdaAmountValue = () => {
    const adaAmountField = this.form.$('adaAmount');
    return adaAmountField.value.length > 0;
  };
  isAddressFromSameWallet = () => {
    const { isAddressFromSameWallet } = this.props;
    const receiverField = this.form.$('receiver');
    return (
      this.hasReceiverValue() &&
      isAddressFromSameWallet &&
      receiverField.isValid
    );
  };
  isDisabled = () => {
    return (
      this.state.isCalculatingTransactionFee ||
      this.state.transactionFee.lte(0) ||
      !this.form.isValid ||
      this.form.validating ||
      this.state.hasPendingRequestTokens ||
      this.requestTokens.length > 0
    );
  };
  setupMobxForm = () =>
    new ReactToolboxMobxForm_1.default(
      {
        fields: {
          receiver: {
            label: this.context.intl.formatMessage(
              messages_1.default.receiverLabel
            ),
            placeholder: this.context.intl.formatMessage(
              messages_1.default.receiverHint
            ),
            value: '',
            validators: [
              async ({ field, form }) => {
                const { value } = field;
                if (value === null || value === '') {
                  this.resetTransactionFee(this.requestTokens.length);
                  this.setReceiverValidity(false);
                  return [
                    false,
                    this.context.intl.formatMessage(
                      messages_1.default.fieldIsRequired
                    ),
                  ];
                }
                const requestToken = this.createRequestToken();
                const payload = {
                  ...this.getTransactionFeePayload(),
                  requestToken,
                };
                const isValid = await this.props.addressValidator(value);
                if (requestToken.aborted) {
                  requestToken.release();
                  return [];
                }
                if (isValid && this.isAddressFromSameWallet()) {
                  this._isAutoFocusEnabled = false;
                }
                this.setReceiverValidity(isValid);
                const adaAmountField = form.$('adaAmount');
                const isAdaAmountValid = adaAmountField.isValid;
                await this.calculateOrResetTransactionFee({
                  isValid: isValid && isAdaAmountValid,
                  requestToken,
                  payload,
                });
                return [
                  isValid,
                  this.context.intl.formatMessage(
                    errors_1.messages.invalidAddress
                  ),
                ];
              },
            ],
          },
          adaAmount: {
            label: this.context.intl.formatMessage(
              messages_1.default.adaAmountLabel
            ),
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
                    this.context.intl.formatMessage(
                      messages_1.default.fieldIsRequired
                    ),
                  ];
                }
                const amountValue = value.toString();
                const requestToken = this.createRequestToken();
                const payload = {
                  ...this.getTransactionFeePayload(),
                  requestToken,
                };
                const isValid = await this.props.validateAmount(
                  (0, formatters_1.formattedAmountToNaturalUnits)(amountValue)
                );
                if (requestToken.aborted) {
                  requestToken.release();
                  return [];
                }
                await this.calculateOrResetTransactionFee({
                  isValid,
                  requestToken,
                  payload,
                });
                return [
                  isValid,
                  this.context.intl.formatMessage(
                    messages_1.default.invalidAmount
                  ),
                ];
              },
            ],
            hooks: {
              onChange: () => {
                return this.setState({
                  transactionFee: new bignumber_js_1.default(0),
                });
              },
            },
          },
          estimatedFee: {
            label: this.context.intl.formatMessage(
              messages_1.default.estimatedFeeLabel
            ),
            placeholder: `0${
              this.getCurrentNumberFormat().decimalSeparator
            }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
            value: null,
          },
        },
      },
      {
        plugins: {
          vjf: (0, VJF_1.default)(),
        },
        options: {
          validateOnBlur: false,
          validateOnChange: true,
          validationDebounceWait: this.props.validationDebounceWait,
        },
      }
    );
  setReceiverValidity(isValid) {
    if (this._isMounted) {
      this.setState(({ isReceiverAddressValidOnce }) => ({
        isReceiverAddressValid: isValid,
        isReceiverAddressValidOnce: isValid || isReceiverAddressValidOnce,
      }));
    }
  }
  validateEmptyAssets = () => {
    return this.selectedAssets
      .filter((_, index) => {
        const quantity = new bignumber_js_1.default(
          this.selectedAssetsAmounts[index]
        );
        return quantity.isZero();
      })
      .forEach(({ uniqueId }) => {
        this.form.$(`asset_${uniqueId}`).validate({
          showErrors: true,
        });
      });
  };
  createRequestToken = () => {
    this.requestTokens.forEach((requestToken) => requestToken.abort());
    const updatePendingRequestTokens = () =>
      this.setState({
        hasPendingRequestTokens: this.requestTokens.length > 0,
      });
    const requestToken = (() => {
      let aborted = false;
      return {
        abort() {
          aborted = true;
        },
        get aborted() {
          return aborted;
        },
        release: () => {
          this.requestTokens = this.requestTokens.filter(
            (r) => r !== requestToken
          );
          updatePendingRequestTokens();
        },
      };
    })();
    this.requestTokens.push(requestToken);
    updatePendingRequestTokens();
    return requestToken;
  };
  getTransactionFeePayload = () => {
    const { form } = this;
    const receiver = form.$('receiver').value;
    const adaAmount = (0, formatters_1.formattedAmountToLovelace)(
      form.$('adaAmount').value
    );
    // @ts-ignore ts-migrate(2322) FIXME: Type '{ policy_id: string; asset_name: string; qua... Remove this comment to see the full error message
    const assets = this.selectedAssets
      .map(({ policyId, assetName }, index) => {
        const quantity = new bignumber_js_1.default(
          this.selectedAssetsAmounts[index]
        );
        return {
          policy_id: policyId,
          asset_name: assetName,
          quantity, // BigNumber or number - prevent parsing a BigNumber to Number (Integer) because of JS number length limitation
        };
      })
      .filter(({ quantity }) => quantity.gt(0));
    return {
      receiver,
      adaAmount,
      assets,
    };
  };
  calculateTransactionFee = async ({
    shouldUpdateMinimumAdaAmount = false,
    requestToken,
    adaAmount,
    receiver,
    assets,
  }) => {
    if (
      !this.state.isReceiverAddressValid ||
      (0, lodash_1.last)(this.requestTokens) !== requestToken ||
      requestToken.aborted
    ) {
      return;
    }
    this.validateEmptyAssets();
    const { selectedAssetUniqueIds } = this.state;
    if (this.requestTokens.length <= 1) {
      this.setState({
        transactionFeeError: null,
        isCalculatingTransactionFee: true,
      });
    }
    try {
      const {
        fee,
        minimumAda,
        coinSelection,
      } = await this.props.calculateTransactionFee(receiver, adaAmount, assets);
      if (this._isMounted && !requestToken.aborted) {
        const minimumAdaValue = minimumAda || new bignumber_js_1.default(0);
        const nextState = {
          minimumAda: minimumAdaValue,
          transactionFee: fee,
          transactionFeeError: null,
          isCalculatingTransactionFee: false,
          adaInputState: this.state.adaInputState,
          coinSelection,
          adaAmount,
        };
        if (shouldUpdateMinimumAdaAmount) {
          const adaInputState = await this.checkAdaInputState(minimumAdaValue);
          nextState.adaInputState = adaInputState;
          this.trySetMinimumAdaAmount(adaInputState, minimumAdaValue);
        }
        this.setState(nextState);
        this.props.onTransactionFeeChange?.(fee);
      }
    } catch (error) {
      if (this._isMounted && !requestToken.aborted) {
        const errorHasLink = !!(0, lodash_1.get)(error, [
          'values',
          'linkLabel',
        ]);
        let transactionFeeError;
        let localizableError = error;
        let values;
        let nextState = {
          isCalculatingTransactionFee: false,
          transactionFee: new bignumber_js_1.default(0),
        };
        if (error.id === 'api.errors.utxoTooSmall') {
          const minimumAda = (0, lodash_1.get)(error, 'values.minimumAda');
          if (minimumAda && !Number.isNaN(Number(minimumAda))) {
            localizableError = selectedAssetUniqueIds.length
              ? messages_1.default.minAdaRequiredWithAssetTooltip
              : messages_1.default.minAdaRequiredWithNoAssetTooltip;
            values = {
              minimumAda,
            };
            if (shouldUpdateMinimumAdaAmount) {
              const minimumAdaValue = new bignumber_js_1.default(minimumAda);
              const adaInputState = await this.checkAdaInputState(
                minimumAdaValue
              );
              this.trySetMinimumAdaAmount(adaInputState, minimumAdaValue);
              this.setState({
                ...nextState,
                adaInputState,
                minimumAda: new bignumber_js_1.default(minimumAda),
              });
              return;
            }
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ minimumAda: BigNumber; isCalculatingTransa... Remove this comment to see the full error message
            nextState = {
              ...nextState,
              minimumAda: new bignumber_js_1.default(minimumAda),
            };
          }
        }
        if (errorHasLink) {
          transactionFeeError = react_1.default.createElement(
            FormattedHTMLMessageWithLink_1.FormattedHTMLMessageWithLink,
            {
              message: localizableError,
              onExternalLinkClick: this.props.onExternalLinkClick,
            }
          );
        } else {
          transactionFeeError = react_1.default.createElement(
            react_intl_1.FormattedHTMLMessage,
            { ...localizableError, values: values }
          );
        }
        this.setState({ ...nextState, transactionFeeError });
      }
    }
  };
  checkAdaInputState = async (minimumAda) => {
    const { adaAmountInputTrack, adaInputState } = this.state;
    if (
      adaAmountInputTrack.gte(minimumAda) &&
      adaInputState === AdaInputStateType.Updated
    ) {
      return AdaInputStateType.Restored;
    }
    if (adaAmountInputTrack.lt(minimumAda)) {
      const isValid = await this.props.validateAmount(
        (0, formatters_1.formattedAmountToNaturalUnits)(minimumAda.toString())
      );
      if (!isValid) {
        return AdaInputStateType.None;
      }
      return AdaInputStateType.Updated;
    }
    return AdaInputStateType.None;
  };
  trySetMinimumAdaAmount = (adaInputState, minimumAda) => {
    const { formFields } = this.state;
    const { adaAmount: adaAmountField } = formFields.receiver;
    switch (adaInputState) {
      case 'updated':
        adaAmountField.onChange(minimumAda.toString());
        break;
      case 'restored':
      case 'reset':
        adaAmountField.onChange(this.state.adaAmountInputTrack.toString());
        break;
      case 'none':
      default:
    }
  };
  updateAdaAmount = async () => {
    const { minimumAda } = this.state;
    const formattedMinimumAda = minimumAda.toString();
    const isValid = await this.props.validateAmount(
      (0, formatters_1.formattedAmountToNaturalUnits)(formattedMinimumAda)
    );
    if (!isValid) {
      return;
    }
    this.form.$('adaAmount').onChange(formattedMinimumAda);
    this.setState({
      adaInputState: AdaInputStateType.None,
      adaAmountInputTrack: minimumAda,
    });
  };
  onAdaAmountFieldChange = (value) => {
    const { formFields } = this.state;
    const { adaAmount: adaAmountField } = formFields.receiver;
    adaAmountField.onChange(value != null ? value : '');
    const adaAmount = new bignumber_js_1.default(value != null ? value : 0);
    this.setState({
      adaAmountInputTrack: adaAmount,
      adaInputState: AdaInputStateType.None,
    });
  };
  isAdaAmountLessThanMinimumRequired = () => {
    const adaAmountField = this.form.$('adaAmount');
    const adaAmount = new bignumber_js_1.default(adaAmountField.value || 0);
    return adaAmount.lt(this.state.minimumAda);
  };
  resetTransactionFee(pendingRequestTokens = 0) {
    if (pendingRequestTokens > 0) {
      return;
    }
    if (this._isMounted) {
      this.setState({
        transactionFee: new bignumber_js_1.default(0),
        transactionFeeError: null,
        isCalculatingTransactionFee: false,
      });
    }
  }
  addAssetRow = (uniqueId) => {
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
  removeAssetRow = (uniqueId) => {
    const { formFields, selectedAssetUniqueIds } = this.state;
    const { receiver } = formFields;
    const assetFields = (0, lodash_1.omit)(receiver.assetFields, uniqueId);
    const assetsDropdown = (0, lodash_1.omit)(
      receiver.assetsDropdown,
      uniqueId
    );
    this.setState(
      {
        selectedAssetUniqueIds: (0, lodash_1.without)(
          selectedAssetUniqueIds,
          uniqueId
        ),
        formFields: {
          ...formFields,
          receiver: { ...receiver, assetFields, assetsDropdown },
        },
      },
      async () => {
        this.removeAssetFields(uniqueId);
        const requestToken = this.createRequestToken();
        const payload = {
          ...this.getTransactionFeePayload(),
          requestToken,
          shouldUpdateMinimumAdaAmount: true,
        };
        this.calculateTransactionFee(payload).finally(requestToken.release);
        this.props.analyticsTracker.sendEvent(
          analytics_1.EventCategories.WALLETS,
          'Removed token from transaction'
        );
      }
    );
  };
  addAssetFields = (uniqueId) => {
    const newAsset = `asset_${uniqueId}`;
    this.form.add({
      name: newAsset,
      value: null,
      key: newAsset,
    });
    this.form
      .$(newAsset)
      .set(
        'label',
        this.context.intl.formatMessage(messages_1.default.assetLabel)
      );
    this.form
      .$(newAsset)
      .set(
        'placeholder',
        `0${this.getCurrentNumberFormat().decimalSeparator}${'0'.repeat(
          this.props.currencyMaxFractionalDigits
        )}`
      );
    this.form.$(newAsset).set('hooks', {
      onChange: () =>
        this.setState({ transactionFee: new bignumber_js_1.default(0) }),
    });
    this.form.$(newAsset).set('validators', [
      async ({ field }) => {
        const { value } = field;
        if (value === null || value === '') {
          this.resetTransactionFee(this.requestTokens.length);
          return [
            false,
            this.context.intl.formatMessage(messages_1.default.fieldIsRequired),
          ];
        }
        const asset = this.getAssetByUniqueId(uniqueId);
        if (!asset) {
          return false;
        }
        const assetValue = new bignumber_js_1.default(
          (0, formatters_1.formattedAmountToNaturalUnits)(field.value)
        );
        const isValidRange =
          assetValue.isGreaterThan(0) &&
          assetValue.isLessThanOrEqualTo(asset.quantity);
        if (!isValidRange) {
          return [
            false,
            this.context.intl.formatMessage(messages_1.default.invalidAmount),
          ];
        }
        const requestToken = this.createRequestToken();
        const payload = {
          ...this.getTransactionFeePayload(),
          requestToken,
          shouldUpdateMinimumAdaAmount: true,
        };
        const amountValue = value.toString();
        const isValid = await this.props.validateAssetAmount(
          (0, formatters_1.formattedAmountToNaturalUnits)(amountValue)
        );
        if (requestToken.aborted) {
          requestToken.release();
          return [];
        }
        await this.calculateOrResetTransactionFee({
          isValid,
          requestToken,
          payload,
        });
        return [
          isValid,
          this.context.intl.formatMessage(messages_1.default.invalidAmount),
        ];
      },
    ]);
    const assetsDropdown = `assetsDropdown_${uniqueId}`;
    this.form.add({
      name: assetsDropdown,
      value: null,
      key: assetsDropdown,
    });
    this.form.$(assetsDropdown).set('type', 'select');
  };
  calculateOrResetTransactionFee = ({ isValid, payload, requestToken }) => {
    if (isValid) {
      return this.calculateTransactionFee(payload).finally(
        requestToken.release
      );
    }
    requestToken.release();
    this.resetTransactionFee(this.requestTokens.length);
  };
  removeAssetFields = (uniqueId) => {
    const assetFieldToDelete = `asset_${uniqueId}`;
    this.form.del(assetFieldToDelete);
    const assetsDropdownFieldToDelete = `assetsDropdown_${uniqueId}`;
    this.form.del(assetsDropdownFieldToDelete);
  };
  onChangeAsset = async (currentUniqueId, newUniqueId) => {
    if (currentUniqueId === newUniqueId) return;
    this.addAssetFields(newUniqueId);
    this.updateFormFields(false, newUniqueId);
    const { selectedAssetUniqueIds: oldSelectedAssetUniqueIds } = this.state;
    const selectedAssetUniqueIds = [...oldSelectedAssetUniqueIds];
    const index = (0, lodash_1.indexOf)(
      selectedAssetUniqueIds,
      currentUniqueId
    );
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
  getMinimumAdaValue = () => {
    const { minimumAda } = this.state;
    return minimumAda.isZero()
      ? walletsConfig_1.TRANSACTION_MIN_ADA_VALUE
      : minimumAda.toFormat();
  };
  renderReceiverRow = () => {
    const { intl } = this.context;
    const {
      formFields,
      transactionFeeError,
      selectedAssetUniqueIds,
      isReceiverAddressValidOnce,
    } = this.state;
    const {
      currencyMaxFractionalDigits,
      walletAmount,
      onTokenPickerDialogOpen,
    } = this.props;
    const {
      adaAmount: adaAmountField,
      receiver: receiverField,
      assetFields,
    } = formFields.receiver;
    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight = selectedAssetUniqueIds.length
      ? assetsSeparatorBasicHeight * (selectedAssetUniqueIds.length + 1) -
        40 * selectedAssetUniqueIds.length
      : assetsSeparatorBasicHeight;
    const minimumAdaValue = this.getMinimumAdaValue();
    const addAssetButtonClasses = (0, classnames_1.default)([
      WalletSendForm_scss_1.default.addAssetButton,
      'primary',
    ]);
    const receiverFieldClasses = (0, classnames_1.default)([
      WalletSendForm_scss_1.default.receiverInput,
      this.isAddressFromSameWallet()
        ? WalletSendForm_scss_1.default.sameReceiverInput
        : null,
    ]);
    const minAdaRequiredTooltip = selectedAssetUniqueIds.length
      ? messages_1.default.minAdaRequiredWithAssetTooltip
      : messages_1.default.minAdaRequiredWithNoAssetTooltip;
    const sameWalletError = intl.formatMessage(
      messages_1.default.sameWalletLabel
    );
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
    return react_1.default.createElement(
      'div',
      { className: WalletSendForm_scss_1.default.fieldsContainer },
      react_1.default.createElement(
        'div',
        { className: receiverFieldClasses },
        react_1.default.createElement(Input_1.Input, {
          ...receiverField.bind(),
          ref: (field) => {
            this.addFocusableField(field);
          },
          className: 'receiver',
          error: receiverFieldError,
          onChange: (value) => {
            receiverField.onChange(value || '');
            this.setState({
              isResetButtonDisabled: false,
            });
          },
          onKeyPress: this.handleSubmitOnEnter,
          themeVariables: receiverFieldThemeVars,
        }),
        this.hasReceiverValue() &&
          react_1.default.createElement(
            'div',
            { className: WalletSendForm_scss_1.default.clearReceiverContainer },
            react_1.default.createElement(ClearButton_1.ClearButton, {
              label: intl.formatMessage(messages_1.default.clearLabel),
              onClick: this.clearReceiverFieldValue,
            })
          )
      ),
      isReceiverAddressValidOnce &&
        react_1.default.createElement(
          react_1.default.Fragment,
          null,
          react_1.default.createElement('div', {
            className: WalletSendForm_scss_1.default.fieldsLine,
            style: {
              height: `${assetsSeparatorCalculatedHeight}px`,
              top: `${assetsSeparatorCalculatedHeight - 10}px`,
              marginTop: `-${assetsSeparatorCalculatedHeight}px`,
            },
          }),
          react_1.default.createElement(
            'div',
            { className: WalletSendForm_scss_1.default.assetInput },
            react_1.default.createElement(
              react_1.Fragment,
              null,
              walletAmount &&
                react_1.default.createElement(
                  'div',
                  { className: WalletSendForm_scss_1.default.amountTokenTotal },
                  intl.formatMessage(messages_1.default.ofLabel),
                  ' ',
                  react_1.default.createElement(
                    discreet_mode_1.DiscreetWalletAmount,
                    { amount: walletAmount }
                  )
                ),
              react_1.default.createElement(
                'div',
                { className: WalletSendForm_scss_1.default.adaAmountLabel },
                intl.formatMessage(global_messages_1.default.adaUnit)
              ),
              react_1.default.createElement(
                'div',
                { className: WalletSendForm_scss_1.default.adaInput },
                react_1.default.createElement(NumericInput_1.NumericInput, {
                  ...adaAmountField.bind(),
                  ref: (field) => {
                    this.addFocusableField(field);
                  },
                  className: 'adaAmount',
                  bigNumberFormat: this.getCurrentNumberFormat(),
                  decimalPlaces: currencyMaxFractionalDigits,
                  numberLocaleOptions: {
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  },
                  onChange: this.onAdaAmountFieldChange,
                  currency: global_messages_1.default.adaUnit,
                  error: adaAmountField.error || transactionFeeError,
                  onKeyPress: this.handleSubmitOnEnter,
                  allowSigns: false,
                  autoFocus: this._isAutoFocusEnabled,
                }),
                this.hasAdaAmountValue() &&
                  react_1.default.createElement(
                    'div',
                    {
                      className:
                        WalletSendForm_scss_1.default.clearAdaContainer,
                    },
                    react_1.default.createElement(ClearButton_1.ClearButton, {
                      label: intl.formatMessage(messages_1.default.clearLabel),
                      onClick: this.clearAdaAmountFieldValue,
                    }),
                    react_1.default.createElement(
                      'div',
                      {
                        className:
                          WalletSendForm_scss_1.default.dividerContainer,
                      },
                      react_1.default.createElement(
                        VerticalSeparator_1.VerticalSeparator,
                        null
                      )
                    )
                  )
              ),
              react_1.default.createElement(
                'div',
                {
                  className: WalletSendForm_scss_1.default.minAdaRequired,
                  'data-testid': 'minimumAdaRequiredMsg',
                },
                this.isAdaAmountLessThanMinimumRequired()
                  ? react_1.default.createElement(
                      react_1.default.Fragment,
                      null,
                      react_1.default.createElement(Button_1.Button, {
                        className: addAssetButtonClasses,
                        label: intl.formatMessage(
                          messages_1.default.updateAdaAmountButton
                        ),
                        onClick: this.updateAdaAmount,
                      }),
                      react_1.default.createElement(
                        'span',
                        null,
                        intl.formatMessage(
                          messages_1.default.updateAdaAmountDescription,
                          {
                            minimumAda: minimumAdaValue,
                          }
                        )
                      )
                    )
                  : react_1.default.createElement(
                      'span',
                      null,
                      intl.formatMessage(messages_1.default.minAdaRequired, {
                        minimumAda: minimumAdaValue,
                      })
                    ),
                react_1.default.createElement(
                  PopOver_1.PopOver,
                  {
                    content: intl.formatMessage(minAdaRequiredTooltip, {
                      minimumAda: minimumAdaValue,
                    }),
                    contentClassName:
                      WalletSendForm_scss_1.default.minAdaTooltipContent,
                    key: 'tooltip',
                  },
                  react_1.default.createElement(react_svg_inline_1.default, {
                    svg: question_mark_inline_svg_1.default,
                    className: WalletSendForm_scss_1.default.infoIcon,
                  })
                )
              )
            ),
            react_1.default.createElement(
              react_1.Fragment,
              null,
              selectedAssetUniqueIds.map((uniqueId, index) =>
                react_1.default.createElement(AssetInput_1.default, {
                  key: uniqueId,
                  uniqueId: uniqueId,
                  // @ts-ignore ts-migrate(2322) FIXME: Type '{ key: string; uniqueId: string; index: numb... Remove this comment to see the full error message
                  index: index,
                  getAssetByUniqueId: this.getAssetByUniqueId,
                  assetFields: assetFields,
                  addFocusableField: this.addFocusableField,
                  currentNumberFormat: this.getCurrentNumberFormat(),
                  removeAssetRow: this.removeAssetRow,
                  handleSubmitOnEnter: this.handleSubmitOnEnter,
                  clearAssetFieldValue: this.clearAssetFieldValue,
                  autoFocus: this._isAutoFocusEnabled,
                })
              )
            ),
            react_1.default.createElement(Button_1.Button, {
              className: addAssetButtonClasses,
              label: intl.formatMessage(messages_1.default.addAssetButtonLabel),
              disabled: !this.hasAvailableAssets,
              onClick: onTokenPickerDialogOpen,
            })
          )
        )
    );
  };
  renderMinimumAmountNotice = (message, values) => {
    return react_1.default.createElement(
      'div',
      {
        className: WalletSendForm_scss_1.default.minimumAmountNotice,
        'data-testid': `WalletSendForm::minimumAmountNotice::${this.state.adaInputState}`,
      },
      react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
        ...message,
        values: values,
      })
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
      selectedAssetUniqueIds,
    } = this.state;
    const {
      assets,
      currencyMaxFractionalDigits,
      hwDeviceStatus,
      isHardwareWallet,
      isDialogOpen,
      isRestoreActive,
      onExternalLinkClick,
      tokenFavorites,
      walletName,
      onTokenPickerDialogClose,
      confirmationDialogData,
    } = this.props;
    const adaAmountField = form.$('adaAmount');
    let fees = '0';
    if (transactionFee.gt(0)) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
    }
    const calculatingFeesSpinnerButtonClasses = (0, classnames_1.default)([
      WalletSendForm_scss_1.default.calculatingFeesSpinnerButton,
      WalletSendForm_scss_1.default.spinning,
    ]);
    const estimatedFeeInputClasses = (0, classnames_1.default)({
      [WalletSendForm_scss_1.default.estimatedFeeInput]: true,
      [WalletSendForm_scss_1.default.withOffset]:
        this.state.adaInputState === AdaInputStateType.Updated ||
        this.state.adaInputState === AdaInputStateType.Restored,
    });
    const minimumAdaValue = this.getMinimumAdaValue();
    return react_1.default.createElement(
      'div',
      { className: WalletSendForm_scss_1.default.component },
      isRestoreActive
        ? react_1.default.createElement(
            'div',
            {
              className:
                WalletSendForm_scss_1.default.syncingTransactionsWrapper,
            },
            react_1.default.createElement(LoadingSpinner_1.default, {
              big: true,
            }),
            react_1.default.createElement(
              'p',
              {
                className:
                  WalletSendForm_scss_1.default.syncingTransactionsText,
              },
              intl.formatMessage(messages_1.default.syncingTransactionsMessage)
            )
          )
        : react_1.default.createElement(
            BorderedBox_1.default,
            null,
            react_1.default.createElement(
              'div',
              { className: WalletSendForm_scss_1.default.walletSendForm },
              formFields.receiver && this.renderReceiverRow(),
              react_1.default.createElement(
                'div',
                { className: estimatedFeeInputClasses },
                react_1.default.createElement(ReadOnlyInput_1.default, {
                  label: intl.formatMessage(
                    messages_1.default.estimatedFeeLabel
                  ),
                  value:
                    fees && !transactionFeeError
                      ? `${fees} ${intl.formatMessage(
                          global_messages_1.default.adaUnit
                        )}`
                      : `0${
                          this.getCurrentNumberFormat().decimalSeparator
                        }${'0'.repeat(
                          this.props.currencyMaxFractionalDigits
                        )} ${intl.formatMessage(
                          global_messages_1.default.adaUnit
                        )}`,
                  isSet: true,
                }),
                (this.state.isCalculatingTransactionFee ||
                  this.state.hasPendingRequestTokens) &&
                  react_1.default.createElement(
                    'div',
                    {
                      className:
                        WalletSendForm_scss_1.default.calculatingFeesContainer,
                      'data-testid': 'transaction-fee-spinner',
                    },
                    react_1.default.createElement(
                      PopOver_1.PopOver,
                      {
                        content: intl.formatMessage(
                          messages_1.default.calculatingFeesLabel
                        ),
                      },
                      react_1.default.createElement('button', {
                        className: calculatingFeesSpinnerButtonClasses,
                      })
                    )
                  )
              ),
              this.state.adaInputState === AdaInputStateType.Updated &&
                this.renderMinimumAmountNotice(
                  messages_1.default.minimumAmountNotice,
                  {
                    minimumAda: minimumAdaValue,
                  }
                ),
              this.state.adaInputState === AdaInputStateType.Restored &&
                this.renderMinimumAmountNotice(
                  messages_1.default.restoredAdaAmount,
                  {
                    minimumAda: minimumAdaValue,
                    adaAmount: adaAmountField.value,
                  }
                ),
              react_1.default.createElement(
                'div',
                { className: WalletSendForm_scss_1.default.buttonsContainer },
                react_1.default.createElement(Button_1.Button, {
                  className: 'flat',
                  label: intl.formatMessage(
                    messages_1.default.resetButtonLabel
                  ),
                  disabled: isResetButtonDisabled,
                  onClick: this.handleOnReset,
                }),
                react_1.default.createElement(Button_1.Button, {
                  className: 'primary',
                  label: intl.formatMessage(messages_1.default.sendButtonLabel),
                  disabled: this.isDisabled(),
                  onClick: this.handleOnSubmit,
                })
              )
            )
          ),
      isDialogOpen(SendConfirmation_view_1.WalletSendConfirmationDialogView) &&
        confirmationDialogData
        ? react_1.default.createElement(
            SendConfirmation_container_1.WalletSendConfirmationDialogContainer,
            {
              receiver: confirmationDialogData.receiver,
              selectedAssets: confirmationDialogData.selectedAssets,
              assetsAmounts: confirmationDialogData.assetsAmounts,
              amount: confirmationDialogData.amount.toFormat(
                currencyMaxFractionalDigits
              ),
              totalAmount: confirmationDialogData.totalAmount,
              transactionFee: confirmationDialogData.transactionFee.toFormat(
                currencyMaxFractionalDigits
              ),
              hwDeviceStatus: hwDeviceStatus,
              isHardwareWallet: isHardwareWallet,
              onExternalLinkClick: onExternalLinkClick,
              formattedTotalAmount: confirmationDialogData.totalAmount.toFormat(
                currencyMaxFractionalDigits
              ),
            }
          )
        : null,
      isDialogOpen(WalletTokenPicker_1.default) &&
        react_1.default.createElement(WalletTokenPicker_1.default, {
          assets: assets,
          previouslyCheckedIds: selectedAssetUniqueIds,
          tokenFavorites: tokenFavorites,
          walletName: walletName,
          onCancel: onTokenPickerDialogClose,
          onAdd: (checked) => {
            onTokenPickerDialogClose();
            checked.forEach(this.addAssetRow);
            this.props.analyticsTracker.sendEvent(
              analytics_1.EventCategories.WALLETS,
              'Added token to transaction'
            );
          },
        })
    );
  }
};
WalletSendForm = __decorate(
  [mobx_react_1.observer, __metadata('design:paramtypes', [Object, Object])],
  WalletSendForm
);
exports.default = WalletSendForm;
//# sourceMappingURL=WalletSendForm.js.map

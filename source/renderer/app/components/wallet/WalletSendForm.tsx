import React, { Component, Fragment } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import type { Field } from 'mobx-react-form';
import { observer } from 'mobx-react';
import { intlShape, FormattedHTMLMessage } from 'react-intl';
import { filter, get, indexOf, omit, map, without, last } from 'lodash';
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
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
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
import { WalletSendConfirmationDialogView } from '../../containers/wallet/dialogs/send-confirmation/SendConfirmation.view';
import { WalletSendConfirmationDialogContainer } from '../../containers/wallet/dialogs/send-confirmation/SendConfirmation.container';
import styles from './WalletSendForm.scss';
import Asset from '../../domains/Asset';
import type { HwDeviceStatus } from '../../domains/Wallet';
import type { AssetToken, ApiTokens } from '../../api/assets/types';
import type { ReactIntlMessage } from '../../types/i18nTypes';
import { DiscreetWalletAmount } from '../../features/discreet-mode';
import WalletTokenPicker from './tokens/wallet-token-picker/WalletTokenPicker';
import { ClearButton } from './widgets/ClearButton';
import { Divider } from './widgets/Divider';
import { CoinSelectionsResponse } from '../../api/transactions/types';

messages.fieldIsRequired = globalMessages.fieldIsRequired;
type AdaInputState = 'restored' | 'updated' | 'reset' | 'none';
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
const AdaInputStateType: EnumMap<string, AdaInputState> = {
  Restored: 'restored',
  Updated: 'updated',
  None: 'none',
  Reset: 'reset',
};

interface TransactionFeePayload {
  receiver: string;
  adaAmount: number;
  assets: ApiTokens;
}

type CalculateTransactionFeeArgs = {
  requestToken: RequestToken;
  shouldUpdateMinimumAdaAmount?: boolean;
} & TransactionFeePayload;

export interface FormData {
  coinSelection: CoinSelectionsResponse;
  receiver: string;
  selectedAssets: AssetToken[];
  assetsAmounts: string[];
  amount: BigNumber;
  totalAmount: BigNumber;
  transactionFee: BigNumber;
  adaAmount: number;
}

export type ConfirmationDialogData = Omit<FormData, 'coinSelection'>;

type Props = {
  currencyMaxIntegerDigits: number;
  currencyMaxFractionalDigits: number;
  currentNumberFormat: string;
  calculateTransactionFee: (
    address: string,
    amount: number,
    selectedAssets: ApiTokens
  ) => Promise<{
    fee: BigNumber;
    coinSelection?: CoinSelectionsResponse;
    minimumAda?: BigNumber;
  }>;
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
  onSubmit: (data: FormData) => any;
  onUnsetActiveAsset: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
  onTransactionFeeChange?: (fee: BigNumber) => void;
  isAddressFromSameWallet: boolean;
  tokenFavorites: Record<string, boolean>;
  walletName: string;
  onTokenPickerDialogOpen: (...args: Array<any>) => any;
  onTokenPickerDialogClose: (...args: Array<any>) => any;
  confirmationDialogData?: ConfirmationDialogData;
  validationDebounceWait?: number;
};

interface FormFields {
  receiver: string;
  adaAmount: string;
  [assets: string]: string;
}

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
  adaAmountInputTrack: BigNumber;
  transactionFee: BigNumber;
  transactionFeeError: (string | null | undefined) | (Node | null | undefined);
  selectedAssetUniqueIds: Array<string>;
  isResetButtonDisabled: boolean;
  isReceiverAddressValid: boolean;
  isReceiverAddressValidOnce: boolean;
  isCalculatingTransactionFee: boolean;
  hasPendingRequestTokens: boolean;
  adaInputState: AdaInputState;
  coinSelection?: CoinSelectionsResponse;
  adaAmount: number;
};

interface RequestToken {
  abort: () => void;
  aborted: boolean;
  release: () => void;
}

@observer
class WalletSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
  };

  state: State = {
    formFields: {} as State['formFields'],
    minimumAda: new BigNumber(0),
    adaAmountInputTrack: new BigNumber(0),
    transactionFee: new BigNumber(0),
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

  requestTokens: RequestToken[] = [];

  form: ReactToolboxMobxForm<FormFields>;

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
  addFocusableField = (field: Input | null | undefined) => {
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
    const adaAmount = new BigNumber(adaAmountField.value || 0);

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
      minimumAda: new BigNumber(0),
      adaAmountInputTrack: new BigNumber(0),
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
  clearAssetFieldValue = (assetField: Field) => {
    if (assetField) {
      assetField.clear();
      this.focusField(assetField);
    }

    this.resetTransactionFee();
  };
  updateFormFields = (resetFormFields: boolean, uniqueId?: string) => {
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
    new ReactToolboxMobxForm<FormFields>(
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
                  this.resetTransactionFee(this.requestTokens.length);
                  this.setReceiverValidity(false);
                  return [
                    false,
                    this.context.intl.formatMessage(messages.fieldIsRequired),
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

                const requestToken = this.createRequestToken();
                const payload = {
                  ...this.getTransactionFeePayload(),
                  requestToken,
                };

                const isValid = await this.props.validateAmount(
                  formattedAmountToNaturalUnits(amountValue)
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
                  this.context.intl.formatMessage(messages.invalidAmount),
                ];
              },
            ],
            hooks: {
              onChange: () => {
                return this.setState({ transactionFee: new BigNumber(0) });
              },
            },
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
          validationDebounceWait: this.props.validationDebounceWait,
        },
      }
    );

  setReceiverValidity(isValid: boolean) {
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
        const quantity = new BigNumber(this.selectedAssetsAmounts[index]);
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

  getTransactionFeePayload = (): TransactionFeePayload => {
    const { form } = this;
    const receiver = form.$('receiver').value;
    const adaAmount = formattedAmountToLovelace(form.$('adaAmount').value);

    // @ts-ignore ts-migrate(2322) FIXME: Type '{ policy_id: string; asset_name: string; qua... Remove this comment to see the full error message
    const assets: ApiTokens = this.selectedAssets
      .map(({ policyId, assetName }, index) => {
        const quantity = new BigNumber(this.selectedAssetsAmounts[index]);
        return {
          policy_id: policyId,
          asset_name: assetName,
          quantity, // BigNumber or number - prevent parsing a BigNumber to Number (Integer) because of JS number length limitation
        };
      })
      .filter(({ quantity }: { quantity: BigNumber }) => quantity.gt(0));

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
  }: CalculateTransactionFeeArgs) => {
    if (
      !this.state.isReceiverAddressValid ||
      last(this.requestTokens) !== requestToken ||
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
        const minimumAdaValue = minimumAda || new BigNumber(0);
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
        const errorHasLink = !!get(error, ['values', 'linkLabel']);
        let transactionFeeError;
        let localizableError = error;
        let values;
        let nextState = {
          isCalculatingTransactionFee: false,
          transactionFee: new BigNumber(0),
        };

        if (error.id === 'api.errors.utxoTooSmall') {
          const minimumAda = get(error, 'values.minimumAda');

          if (minimumAda && !Number.isNaN(Number(minimumAda))) {
            localizableError = selectedAssetUniqueIds.length
              ? messages.minAdaRequiredWithAssetTooltip
              : messages.minAdaRequiredWithNoAssetTooltip;
            values = {
              minimumAda,
            };

            if (shouldUpdateMinimumAdaAmount) {
              const minimumAdaValue = new BigNumber(minimumAda);
              const adaInputState = await this.checkAdaInputState(
                minimumAdaValue
              );
              this.trySetMinimumAdaAmount(adaInputState, minimumAdaValue);
              this.setState({
                ...nextState,
                adaInputState,
                minimumAda: new BigNumber(minimumAda),
              });
              return;
            }

            // @ts-ignore ts-migrate(2322) FIXME: Type '{ minimumAda: BigNumber; isCalculatingTransa... Remove this comment to see the full error message
            nextState = { ...nextState, minimumAda: new BigNumber(minimumAda) };
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

        this.setState({ ...nextState, transactionFeeError });
      }
    }
  };
  checkAdaInputState = async (
    minimumAda: BigNumber
  ): Promise<AdaInputState> => {
    const {
      adaAmountInputTrack,
      selectedAssetUniqueIds,
      adaInputState,
    } = this.state;

    if (
      adaAmountInputTrack.gte(minimumAda) &&
      adaInputState === AdaInputStateType.Updated
    ) {
      return AdaInputStateType.Restored;
    }

    if (adaAmountInputTrack.lt(minimumAda)) {
      const isValid = await this.props.validateAmount(
        formattedAmountToNaturalUnits(minimumAda.toString())
      );

      if (!isValid) {
        return AdaInputStateType.None;
      }

      return AdaInputStateType.Updated;
    }

    return AdaInputStateType.None;
  };
  trySetMinimumAdaAmount = (
    adaInputState: AdaInputState,
    minimumAda: BigNumber
  ) => {
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
      formattedAmountToNaturalUnits(formattedMinimumAda)
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
  onAdaAmountFieldChange = (value: string) => {
    const { formFields } = this.state;
    const { adaAmount: adaAmountField } = formFields.receiver;
    adaAmountField.onChange(value != null ? value : '');
    const adaAmount = new BigNumber(value != null ? value : 0);
    this.setState({
      adaAmountInputTrack: adaAmount,
      adaInputState: AdaInputStateType.None,
    });
  };
  isAdaAmountLessThanMinimumRequired = () => {
    const adaAmountField = this.form.$('adaAmount');
    const adaAmount = new BigNumber(adaAmountField.value || 0);
    return adaAmount.lt(this.state.minimumAda);
  };

  resetTransactionFee(pendingRequestTokens = 0) {
    if (pendingRequestTokens > 0) {
      return;
    }

    if (this._isMounted) {
      this.setState({
        transactionFee: new BigNumber(0),
        transactionFeeError: null,
        isCalculatingTransactionFee: false,
      });
    }
  }

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
    const { receiver } = formFields;
    const assetFields = omit(receiver.assetFields, uniqueId);
    const assetsDropdown = omit(receiver.assetsDropdown, uniqueId);
    this.setState(
      {
        selectedAssetUniqueIds: without(selectedAssetUniqueIds, uniqueId),
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
      }
    );
  };
  addAssetFields = (uniqueId: string) => {
    const newAsset = `asset_${uniqueId}`;
    this.form.add({
      name: newAsset,
      value: null,
      key: newAsset,
    });
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

    this.form.$(newAsset).set('hooks', {
      onChange: () => this.setState({ transactionFee: new BigNumber(0) }),
    });

    this.form.$(newAsset).set('validators', [
      async ({ field }) => {
        const { value } = field;

        if (value === null || value === '') {
          this.resetTransactionFee(this.requestTokens.length);
          return [
            false,
            this.context.intl.formatMessage(messages.fieldIsRequired),
          ];
        }

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

        if (!isValidRange) {
          return [
            false,
            this.context.intl.formatMessage(messages.invalidAmount),
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
          formattedAmountToNaturalUnits(amountValue)
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
          this.context.intl.formatMessage(messages.invalidAmount),
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

  calculateOrResetTransactionFee = ({
    isValid,
    payload,
    requestToken,
  }: {
    isValid: boolean;
    payload: CalculateTransactionFeeArgs;
    requestToken: RequestToken;
  }) => {
    if (isValid) {
      return this.calculateTransactionFee(payload).finally(
        requestToken.release
      );
    }
    requestToken.release();
    this.resetTransactionFee(this.requestTokens.length);
  };

  removeAssetFields = (uniqueId: string) => {
    const assetFieldToDelete = `asset_${uniqueId}`;
    this.form.del(assetFieldToDelete);
    const assetsDropdownFieldToDelete = `assetsDropdown_${uniqueId}`;
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
  getMinimumAdaValue = () => {
    const { minimumAda } = this.state;
    return minimumAda.isZero()
      ? TRANSACTION_MIN_ADA_VALUE
      : minimumAda.toFormat();
  };
  renderReceiverRow = (): Node => {
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
    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
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
              <ClearButton
                label={intl.formatMessage(messages.clearLabel)}
                onClick={this.clearReceiverFieldValue}
              />
            </div>
          )}
        </div>
        {isReceiverAddressValidOnce && (
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
                <div className={styles.adaInput}>
                  <NumericInput
                    {...adaAmountField.bind()}
                    ref={(field) => {
                      this.addFocusableField(field);
                    }}
                    className="adaAmount"
                    bigNumberFormat={this.getCurrentNumberFormat()}
                    decimalPlaces={currencyMaxFractionalDigits}
                    numberLocaleOptions={{
                      minimumFractionDigits: currencyMaxFractionalDigits,
                    }}
                    onChange={this.onAdaAmountFieldChange}
                    currency={globalMessages.adaUnit}
                    error={adaAmountField.error || transactionFeeError}
                    onKeyPress={this.handleSubmitOnEnter}
                    allowSigns={false}
                    autoFocus={this._isAutoFocusEnabled}
                  />
                  {this.hasAdaAmountValue() && (
                    <div className={styles.clearAdaContainer}>
                      <ClearButton
                        label={intl.formatMessage(messages.clearLabel)}
                        onClick={this.clearAdaAmountFieldValue}
                      />
                      <div className={styles.dividerContainer}>
                        <Divider />
                      </div>
                    </div>
                  )}
                </div>
                <div
                  className={styles.minAdaRequired}
                  data-testid="minimumAdaRequiredMsg"
                >
                  {this.isAdaAmountLessThanMinimumRequired() ? (
                    <>
                      <Button
                        className={addAssetButtonClasses}
                        label={intl.formatMessage(
                          messages.updateAdaAmountButton
                        )}
                        onClick={this.updateAdaAmount}
                      />
                      <span>
                        {intl.formatMessage(
                          messages.updateAdaAmountDescription,
                          {
                            minimumAda: minimumAdaValue,
                          }
                        )}
                      </span>
                    </>
                  ) : (
                    <span>
                      {intl.formatMessage(messages.minAdaRequired, {
                        minimumAda: minimumAdaValue,
                      })}
                    </span>
                  )}

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
                      // @ts-ignore ts-migrate(2322) FIXME: Type '{ key: string; uniqueId: string; index: numb... Remove this comment to see the full error message
                      index={index}
                      getAssetByUniqueId={this.getAssetByUniqueId}
                      assetFields={assetFields}
                      addFocusableField={this.addFocusableField}
                      currentNumberFormat={this.getCurrentNumberFormat()}
                      removeAssetRow={this.removeAssetRow}
                      handleSubmitOnEnter={this.handleSubmitOnEnter}
                      clearAssetFieldValue={this.clearAssetFieldValue}
                      autoFocus={this._isAutoFocusEnabled}
                    />
                  )
                )}
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                disabled={!this.hasAvailableAssets}
                onClick={onTokenPickerDialogOpen}
              />
            </div>
          </>
        )}
      </div>
    );
  };
  renderMinimumAmountNotice = (message: ReactIntlMessage, values: {}) => {
    return (
      <div
        className={styles.minimumAmountNotice}
        data-testid={`WalletSendForm::minimumAmountNotice::${this.state.adaInputState}`}
      >
        <FormattedHTMLMessage {...message} values={values} />
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

    const calculatingFeesSpinnerButtonClasses = classNames([
      styles.calculatingFeesSpinnerButton,
      styles.spinning,
    ]);
    const estimatedFeeInputClasses = classNames({
      [styles.estimatedFeeInput]: true,
      [styles.withOffset]:
        this.state.adaInputState === AdaInputStateType.Updated ||
        this.state.adaInputState === AdaInputStateType.Restored,
    });
    const minimumAdaValue = this.getMinimumAdaValue();
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
              {formFields.receiver && this.renderReceiverRow()}
              <div className={estimatedFeeInputClasses}>
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
                {(this.state.isCalculatingTransactionFee ||
                  this.state.hasPendingRequestTokens) && (
                  <div
                    className={styles.calculatingFeesContainer}
                    data-testid="transaction-fee-spinner"
                  >
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
              {this.state.adaInputState === AdaInputStateType.Updated &&
                this.renderMinimumAmountNotice(messages.minimumAmountNotice, {
                  minimumAda: minimumAdaValue,
                })}
              {this.state.adaInputState === AdaInputStateType.Restored &&
                this.renderMinimumAmountNotice(messages.restoredAdaAmount, {
                  minimumAda: minimumAdaValue,
                  adaAmount: adaAmountField.value,
                })}
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
        {isDialogOpen(WalletSendConfirmationDialogView) &&
        confirmationDialogData ? (
          <WalletSendConfirmationDialogContainer
            receiver={confirmationDialogData.receiver}
            selectedAssets={confirmationDialogData.selectedAssets}
            assetsAmounts={confirmationDialogData.assetsAmounts}
            amount={confirmationDialogData.amount.toFormat(
              currencyMaxFractionalDigits
            )}
            totalAmount={confirmationDialogData.totalAmount}
            transactionFee={confirmationDialogData.transactionFee.toFormat(
              currencyMaxFractionalDigits
            )}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            onExternalLinkClick={onExternalLinkClick}
            formattedTotalAmount={confirmationDialogData.totalAmount.toFormat(
              currencyMaxFractionalDigits
            )}
          />
        ) : null}
        {isDialogOpen(WalletTokenPicker) && (
          <WalletTokenPicker
            assets={assets}
            previouslyCheckedIds={selectedAssetUniqueIds}
            tokenFavorites={tokenFavorites}
            walletName={walletName}
            onCancel={onTokenPickerDialogClose}
            onAdd={(checked) => {
              onTokenPickerDialogClose();
              checked.forEach(this.addAssetRow);
            }}
          />
        )}
      </div>
    );
  }
}

export default WalletSendForm;

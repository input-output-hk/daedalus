// @flow
import React, { Component, Fragment } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { Button } from 'react-polymorph/lib/components/Button';
import { Input } from 'react-polymorph/lib/components/Input';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { defineMessages, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import BigNumber from 'bignumber.js';
import { get, orderBy, map } from 'lodash';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import SVGInline from 'react-svg-inline';
import classNames from 'classnames';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../utils/form';
import AmountInputSkin from './skins/AmountInputSkin';
import BorderedBox from '../widgets/BorderedBox';
import LoadingSpinner from '../widgets/LoadingSpinner';
import styles from './WalletAssetsSendForm.scss';
import globalMessages from '../../i18n/global-messages';
import WalletSendConfirmationDialogContainer from '../../containers/wallet/dialogs/WalletSendConfirmationDialogContainer';
import {
  formattedAmountToNaturalUnits,
  formattedAmountToLovelace,
  formattedWalletAmount,
  formattedTokenWalletAmount,
} from '../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { FormattedHTMLMessageWithLink } from '../widgets/FormattedHTMLMessageWithLink';
import { NUMBER_FORMATS } from '../../../../common/types/number.types';
/* eslint-disable consistent-return */
import { messages as apiErrorMessages } from '../../api/errors';
import type { HwDeviceStatus } from '../../domains/Wallet';
import WalletAssetsSendConfirmationDialog from './WalletAssetsSendConfirmationDialog';
import closeIcon from '../../assets/images/close-cross.inline.svg';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';
import ReadOnlyInput from '../widgets/forms/ReadOnlyInput';
import Asset from '../../domains/Asset';
import type { AssetItems, WalletSummaryAsset } from '../../api/assets/types';
import questionMarkIcon from '../../assets/images/question-mark.inline.svg';
import { TRANSACTION_MIN_ADA_VALUE } from '../../config/walletsConfig';

export const messages = defineMessages({
  titleLabel: {
    id: 'wallet.send.form.title.label',
    defaultMessage: '!!!Title',
    description: 'Label for the "Title" text input in the wallet send form.',
  },
  titleHint: {
    id: 'wallet.send.form.title.hint',
    defaultMessage: '!!!E.g: Money for Frank',
    description:
      'Hint inside the "Receiver" text input in the wallet send form.',
  },
  receiverLabel: {
    id: 'wallet.send.form.receiver.label',
    defaultMessage: '!!!Receiver',
    description: 'Label for the "Receiver" text input in the wallet send form.',
  },
  receiverHint: {
    id: 'wallet.send.form.receiver.placeholder',
    defaultMessage: '!!!Paste an address',
    description:
      'Hint inside the "Receiver" text input in the wallet send form.',
  },
  assetLabel: {
    id: 'wallet.send.form.asset.label',
    defaultMessage: '!!!Token',
    description: 'Label for the "Token" number input in the wallet send form.',
  },
  assetAdaLabel: {
    id: 'wallet.send.form.asset.adaLabel',
    defaultMessage: '!!!Ada',
    description: 'Label for the "Ada" input in the wallet send form.',
  },
  removeReceiverButtonLabel: {
    id: 'wallet.send.form.button.removeReceiver',
    defaultMessage: '!!!Remove',
    description: 'Label for the "Remove" button in the wallet send form.',
  },
  addAssetButtonLabel: {
    id: 'wallet.send.form.button.addAssetButtonLabel',
    defaultMessage: '!!!+ Add a token',
    description:
      'Label for the "+ Add a token" button in the wallet send form.',
  },
  estimatedFeeLabel: {
    id: 'wallet.send.form.estimatedFee.label',
    defaultMessage: '!!!Estimated fees',
    description:
      'Label for the "Estimated fees" number input in the wallet send form.',
  },
  ofLabel: {
    id: 'wallet.send.form.of.label',
    defaultMessage: '!!!of',
    description: 'Label for the "of" max ADA value in the wallet send form.',
  },
  minAdaRequired: {
    id: 'wallet.send.form.minAdaRequired',
    defaultMessage: '!!!a minimum of {minimumAda} ADA required',
    description:
      'Label for the min ADA required value in the wallet send form.',
  },
  minAdaRequiredTooltip: {
    id: 'wallet.send.form.minAdaRequiredTooltip',
    defaultMessage:
      '!!!A minimum of {minimumAda} ADA needs to be sent to this receiver since you are sending other assets.',
    description:
      'Tooltip for the min ADA required value in the wallet send form.',
  },
  descriptionLabel: {
    id: 'wallet.send.form.description.label',
    defaultMessage: '!!!Description',
    description:
      'Label for the "description" text area in the wallet send form.',
  },
  descriptionHint: {
    id: 'wallet.send.form.description.hint',
    defaultMessage: '!!!You can add a message if you want',
    description: 'Hint in the "description" text area in the wallet send form.',
  },
  resetButtonLabel: {
    id: 'wallet.send.form.reset',
    defaultMessage: '!!!Reset',
    description: 'Label for the reset button on the wallet send form.',
  },
  sendButtonLabel: {
    id: 'wallet.send.form.send',
    defaultMessage: '!!!Send',
    description: 'Label for the send button on the wallet send form.',
  },
  invalidAmount: {
    id: 'wallet.send.form.errors.invalidAmount',
    defaultMessage: '!!!Please enter a valid amount.',
    description: 'Error message shown when invalid amount was entered.',
  },
  invalidTitle: {
    id: 'wallet.send.form.errors.invalidTitle',
    defaultMessage: '!!!Please enter a title with at least 3 characters.',
    description:
      'Error message shown when invalid transaction title was entered.',
  },
  syncingTransactionsMessage: {
    id: 'wallet.send.form.syncingTransactionsMessage',
    defaultMessage:
      '!!!This wallet is currently being synced with the blockchain. While synchronisation is in progress transacting is not possible and transaction history is not complete.',
    description:
      'Syncing transactions message shown during async wallet restore in the wallet send form.',
  },
  calculatingFeesLabel: {
    id: 'wallet.send.form.calculatingFeesLabel',
    defaultMessage: '!!!Calculating fees',
    description:
      'Label for the "Calculating fees" message for amount input field.',
  },
  syncingWallet: {
    id: 'wallet.send.form.syncingWallet',
    defaultMessage: '!!!syncing',
    description: 'Syncing wallet label on the send tokens form.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  currencyUnit: string,
  currencyMaxIntegerDigits?: number,
  currencyMaxFractionalDigits: number,
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>,
  calculateTransactionFee: (
    address: string,
    amount: number,
    assets?: AssetItems
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
  selectedToken: ?Asset,
  unsetActiveTokenFingerprint: Function,
};

type State = {
  isTransactionFeeCalculated: boolean,
  transactionFee: BigNumber,
  feeCalculationRequestQue: number,
  transactionFeeError: ?string | ?Node,
  assetsError: ?Array<?string | ?Node>,
  showAssetRemoveBtn: Array<boolean>,
  sendFormFields: Object,
  selectedAssetFingerprints: Array<string>,
  isResetButtonDisabled: boolean,
  minimumAda: BigNumber,
  isReceiverAddressValid: boolean,
};

@observer
export default class WalletAssetsSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isResetButtonDisabled: true,
    isReceiverAddressValid: false,
    isTransactionFeeCalculated: false,
    minimumAda: new BigNumber(0),
    transactionFee: new BigNumber(0),
    transactionFeeError: null,
    feeCalculationRequestQue: 0,
    selectedAssetFingerprints: [],
    assetsError: null,
    showAssetRemoveBtn: [],
    sendFormFields: {},
  };

  // We need to track the fee calculation state in order to disable
  // the "Submit" button as soon as either receiver or amount field changes.
  // This is required as we are using debounced validation and we need to
  // disable the "Submit" button as soon as the value changes and then wait for
  // the validation to end in order to see if the button should be enabled or not.
  _isCalculatingTransactionFee = false;
  _isCalculatingAssetsFee = false;

  // We need to track the mounted state in order to avoid calling
  // setState promise handling code after the component was already unmounted:
  // Read more: https://facebook.github.io/react/blog/2015/12/16/ismounted-antipattern.html
  _isMounted = false;

  componentDidMount() {
    this._isMounted = true;
    const { assets, selectedToken } = this.props;
    this.setFormFields(false, assets && assets.length ? assets[0].fingerprint : null, 1);
    if (selectedToken) {
      setTimeout(() => {
        const { sendFormFields } = this.state;
        const { receiver } = sendFormFields;
        const { selectedNativeTokens, formAssets/* , walletsDropdown */ } = receiver;
        const id = formAssets.length + 1;
        const availableAssets = this.filterAvailableAssets(selectedNativeTokens, assets);
        this.addNewAssetRow(
          id,
          availableAssets[0].fingerprint,
          availableAssets[0]
        );
      }, 2000);
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
    this.props.unsetActiveTokenFingerprint();
  }

  get transactionAssetsAmounts() {
    const { sendFormFields } = this.state;
    const assetsFields = get(sendFormFields, 'receiver.formAssets');
    return map(assetsFields, (assetField) => assetField.value);
  }

  get transactionAssets() {
    const { selectedAssetFingerprints } = this.state || [];
    const { assets } = this.props;
    return map(selectedAssetFingerprints, (fingerprint) =>
      assets.find((asset) => asset.fingerprint === fingerprint)
    );
  }

  handleOnSubmit = () => {
    if (this.isDisabled()) {
      return false;
    }
    this.props.openDialogAction({
      dialog: WalletAssetsSendConfirmationDialog,
    });
  };

  clearReceiverAddress = () => {
    const receiverField = this.form.$('receiver');
    if (receiverField) {
      receiverField.clear();
      this.setReceiverValidity(true);
    }
  };

  clearAssetValue = (singleAsset?: any) => {
    const assetField = singleAsset || this.form.$('assetField');
    if (assetField) {
      assetField.clear();
    }
  };

  handleOnReset = () => {
    this.form.reset();
    this.disableResetButton();
    this.hideReceiverField();
    const { sendFormFields } = this.state;
    const { receiver } = sendFormFields;
    if (receiver) {
      receiver.formAssets.map((singleAsset) =>
        this.clearAssetValue(singleAsset)
      );
    }
    this.clearReceiverAddress();
    this.setFormFields(true);
  };

  disableResetButton = () => {
    this.setState({
      isResetButtonDisabled: true,
    });
  };

  setFormFields = (
    resetFormFields: boolean,
    fingerprint?: string,
    id: number,
    selectedNativeToken?: Asset,
  ) => {
    const formFields = this.form.fields;
    const receiverField = formFields.get(`receiver`);
    const assetAdaField = formFields.get(`adaField`);
    const assetField = fingerprint
      ? formFields.get(`asset_${fingerprint}_${id}`)
      : null;
    const walletsDropdownField = fingerprint
      ? formFields.get(`walletsDropdown_${fingerprint}_${id}`)
      : null;
    if (resetFormFields) {
      this.setState({
        sendFormFields: {
          receiver: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            formAssets: [],
            walletsDropdown: [],
            selectedNativeTokens: [],
          },
        },
      });
    } else {
      let currentAssets = [];
      let currentWalletsDropdown = [];
      let currentSelectedNativeTokens = [];
      const { sendFormFields } = this.state;
      const { receiver } = sendFormFields;
      let receiverFieldItem = null;
      if (receiver) {
        receiverFieldItem = receiver.receiver;
      }
      if (receiverFieldItem) {
        currentAssets = receiver.formAssets;
        currentWalletsDropdown = receiver.walletsDropdown;
        currentSelectedNativeTokens =
          receiver.selectedNativeTokens;
      }
      if (assetField) {
        if (!currentAssets) {
          currentAssets = [];
        }
        currentAssets.push(assetField);
      }
      if (walletsDropdownField) {
        if (!currentWalletsDropdown) {
          currentWalletsDropdown = [];
        }
        currentWalletsDropdown.push(walletsDropdownField);
      }
      if (selectedNativeToken) {
        if (!currentSelectedNativeTokens) {
          currentSelectedNativeTokens = [];
        }
        currentSelectedNativeTokens.push(selectedNativeToken);
      }
      this.setState((prevState) => ({
        sendFormFields: {
          ...prevState.sendFormFields,
          receiver: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            formAssets: currentAssets,
            walletsDropdown: currentWalletsDropdown,
            selectedNativeTokens: currentSelectedNativeTokens,
          },
        },
      }));
    }
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.handleOnSubmit);

  isDisabled = () =>
    this._isCalculatingTransactionFee ||
    this._isCalculatingAssetsFee ||
    !this.state.isTransactionFeeCalculated;

  // FORM VALIDATION
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
        adaField: {
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
                this.calculateTransactionFee(receiverValue, amountValue);
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
        assetField: {
          label: this.context.intl.formatMessage(messages.assetLabel),
          value: '',
          validators: [
            async ({ field, form }) => {
              if (field.value === null) {
                this.resetTransactionFee();
                return [
                  false,
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
              const { sendFormFields } = this.state;
              const { receiver } = sendFormFields;
              let selectedNativeTokens;
              if (receiver) {
                selectedNativeTokens = receiver.selectedNativeTokens;
              }
              if (isValid && isReceiverValid) {
                let assets = [];
                if (selectedNativeTokens && selectedNativeTokens.length) {
                  assets = selectedNativeTokens.map((item) => {
                    return {
                      policy_id: item.policyId,
                      asset_name: item.assetName,
                      quantity: item.quantity.toNumber(),
                    };
                  });
                }
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
        walletsDropdownField: {
          type: 'select',
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

  resetTransactionFee() {
    if (this._isMounted) {
      this._isCalculatingTransactionFee = false;
      this._isCalculatingAssetsFee = false;
      this.setState({
        isTransactionFeeCalculated: false,
        transactionFee: new BigNumber(0),
        transactionFeeError: null,
        assetsError: null,
      });
    }
  }

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
    assets?: AssetItems
  ) => {
    const amount = formattedAmountToLovelace(amountValue);
    const {
      feeCalculationRequestQue: prevFeeCalculationRequestQue,
    } = this.state;
    this.setState((prevState) => ({
      isTransactionFeeCalculated: false,
      transactionFee: new BigNumber(0),
      transactionFeeError: null,
      assetsError: null,
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

  getCurrentNumberFormat() {
    return NUMBER_FORMATS[this.props.currentNumberFormat];
  }

  hideReceiverField = () => {
    this.form.del('receiver');
  };

  hasReceiverValue = () => {
    const receiverField = this.form.$(`receiver`);
    return receiverField.value.length > 0;
  };

  hasAssetValue = (asset: any) => {
    return asset && asset.value;
  };

  showAssetRemoveButton = (assetId: number) => {
    const { showAssetRemoveBtn } = this.state;
    showAssetRemoveBtn[assetId] = true;
    this.setState({
      showAssetRemoveBtn,
    });
  };

  hideAssetRemoveButton = (assetId: number) => {
    const { showAssetRemoveBtn } = this.state;
    showAssetRemoveBtn[assetId] = false;
    this.setState({
      showAssetRemoveBtn,
    });
  };

  filterAvailableAssets = (selectedNativeTokens: Array<string>, assets: Array<WalletSummaryAsset>) => {
    const availableAssets = [];
    selectedNativeTokens.map((item) => {
      return availableAssets.push(
        ...assets.filter(
          (asset) => asset.fingerprint !== item.fingerprint
        )
      );
    });
    return availableAssets.length ? availableAssets : assets;
  };

  renderReceiverRow = (index: number): Node => {
    const { intl } = this.context;
    const {
      isClearTooltipOpeningDownward,
      currencyMaxFractionalDigits,
      walletAmount,
      isHardwareWallet,
      assets,
    } = this.props;

    const {
      selectedAssetFingerprints,
      isTransactionFeeCalculated,
      transactionFee,
      transactionFeeError,
      assetsError,
      sendFormFields,
      minimumAda,
      isReceiverAddressValid,
    } = this.state;

    const {
      receiver,
    } = sendFormFields;

    const {
      receiver: receiverField,
      adaAsset,
      formAssets,
      walletsDropdown,
      selectedNativeTokens
    } = receiver;

    const adaAssetFieldProps = adaAsset.bind();

    const assetFieldProps = formAssets.map((singleAsset) => singleAsset.bind());

    const walletsDropdownFieldProps = walletsDropdown.map((dropdown) =>
      dropdown.bind()
    );

    const estimatedField = this.form.$('estimatedFee');

    const amount = assetFieldProps.map((assetField) =>
      assetField && assetField.value ? new BigNumber(assetField.value) : ''
    );

    const receiverLabel = intl.formatMessage(messages.receiverLabel);

    receiverField.set('label', receiverLabel);

    let fees = null;
    if (isTransactionFeeCalculated && transactionFee) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
    }

    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight =
      formAssets && formAssets.length
        ? assetsSeparatorBasicHeight * (formAssets.length + 1) - 40 * formAssets.length
        : assetsSeparatorBasicHeight;

    const minimumAdaValue = minimumAda.isZero()
      ? TRANSACTION_MIN_ADA_VALUE
      : minimumAda.toFormat();

    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      /* !filteredAssets.length ||
      filteredAssets[filteredAssets.length - 1].length === 1
        ? styles.disabled
        : null, */
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
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {this.hasReceiverValue() && (
            <div className={styles.clearReceiverContainer}>
              <PopOver
                content="Clear"
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
                <div className={styles.adaAssetLabel}>
                  {intl.formatMessage(globalMessages.unitAda)}
                </div>
                <NumericInput
                  {...adaAssetFieldProps}
                  className="adaAsset"
                  value={adaAsset.value}
                  label={`${intl.formatMessage(messages.assetAdaLabel)}`}
                  bigNumberFormat={this.getCurrentNumberFormat()}
                  decimalPlaces={currencyMaxFractionalDigits}
                  numberLocaleOptions={{
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  }}
                  onChange={(value) => {
                    this._isCalculatingTransactionFee = true;
                    adaAsset.onChange(value);
                    estimatedField.onChange(fees);
                  }}
                  currency={globalMessages.unitAda}
                  error={adaAsset.error || transactionFeeError}
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
                {formAssets.map((singleAsset: any, assetIndex: number) => {
                  const token = selectedNativeTokens && selectedNativeTokens.length ? selectedNativeTokens[assetIndex] : {};
                  const { quantity, metadata } = token;
                  // @Todo - Fix available assets filtering
                  // const availableAssets = selectedNativeTokens && selectedNativeTokens ? this.filterAvailableAssets(selectedNativeTokens, assets) : assets;
                  const availableAssets =  assets;
                  const sortedAssets = orderBy(availableAssets, 'metadata.acronym', 'asc');

                  return (
                    <div
                      key={`receiver_asset_${singleAsset.fingerprint}_${assetIndex}`}
                      onMouseEnter={() =>
                        this.showAssetRemoveButton(assetIndex)
                      }
                      onMouseLeave={() =>
                        this.hideAssetRemoveButton(assetIndex)
                      }
                      className={styles.fieldContainer}
                    >
                      {quantity && quantity.isPositive() && (
                        <div className={styles.amountTokenTotal}>
                          {intl.formatMessage(messages.ofLabel)}&nbsp;
                          {formattedTokenWalletAmount(quantity, metadata)}
                        </div>
                      )}
                      <NumericInput
                        {...assetFieldProps[assetIndex]}
                        placeholder={
                          metadata && metadata.unit
                            ? `0${
                              this.getCurrentNumberFormat().decimalSeparator
                            }${'0'.repeat(metadata.unit.decimals)}`
                            : '0'
                        }
                        className={classNames([
                          styles.assetItem,
                          this.state.showAssetRemoveBtn &&
                          this.state.showAssetRemoveBtn[assetIndex]
                            ? styles.hasButton
                            : null,
                        ])}
                        label={
                          <>
                            {`${intl.formatMessage(messages.assetLabel)} #${
                              assetIndex + 1
                            }`}
                            <Button
                              className={classNames([
                                styles.removeAssetButton,
                                'flat',
                                this.state.showAssetRemoveBtn &&
                                this.state.showAssetRemoveBtn[assetIndex]
                                  ? styles.active
                                  : null,
                              ])}
                              label={intl.formatMessage(
                                messages.removeReceiverButtonLabel
                              )}
                              onClick={() =>
                                this.removeAssetRow(
                                  singleAsset.name,
                                  assetIndex
                                )
                              }
                              skin={ButtonSkin}
                            />
                          </>
                        }
                        bigNumberFormat={
                          metadata && metadata.unit
                            ? this.getCurrentNumberFormat()
                            : null
                        }
                        decimalPlaces={
                          metadata && metadata.unit ? metadata.unit.decimals : 0
                        }
                        numberLocaleOptions={{
                          minimumFractionDigits:
                            metadata && metadata.unit
                              ? metadata.unit.decimals
                              : 0,
                        }}
                        onChange={(value) => {
                          this._isCalculatingAssetsFee = true;
                          this.setState({
                            isResetButtonDisabled: false,
                          });
                          formAssets[assetIndex].onChange(value);
                          estimatedField.onChange(fees);
                        }}
                        currency={metadata ? metadata.acronym : null}
                        value={amount[assetIndex]}
                        error={assetsError && assetsError[assetIndex]
                          ? assetsError[assetIndex]
                          : null
                        }
                        skin={AmountInputSkin}
                        onKeyPress={(
                          evt: SyntheticKeyboardEvent<EventTarget>
                        ) => {
                          const {charCode} = evt;
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
                        {this.hasAssetValue(assetFieldProps[assetIndex]) && (
                          <div className={styles.clearAssetContainer}>
                            <PopOver
                              content="Clear"
                              placement={
                                isClearTooltipOpeningDownward ? 'bottom' : 'top'
                              }
                            >
                              <button
                                onClick={() =>
                                  this.clearAssetValue(singleAsset)
                                }
                                className={styles.clearAssetButton}
                              >
                                <SVGInline
                                  svg={closeIcon}
                                  className={styles.clearReceiverIcon}
                                />
                              </button>
                            </PopOver>
                            <div className={styles.separator}/>
                          </div>
                        )}
                        <div
                          className={classNames([
                            styles.walletsDropdownWrapper,
                            this.hasAssetValue(assetFieldProps[assetIndex])
                              ? styles.hasValue
                              : null,
                          ])}
                        >
                          <WalletsDropdown
                            className={styles.walletsDropdown}
                            {...walletsDropdownFieldProps[index]}
                            numberOfStakePools={4}
                            assets={sortedAssets}
                            onChange={(fingerprint) => {
                              this.onSelectAsset(index, fingerprint);
                              this.updateSelectedNativeTokens(
                                fingerprint,
                                index
                              );
                            }}
                            syncingLabel={intl.formatMessage(
                              messages.syncingWallet
                            )}
                            hasAssetsEnabled
                            value={selectedAssetFingerprints[assetIndex]}
                            getStakePoolById={() => {
                            }}
                            errorPosition="bottom"
                          />
                        </div>
                      </div>
                    </div>
                  );
                })}
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                disabled={isHardwareWallet || !assets.length}
                onClick={() => {
                  const id = formAssets.length + 1;
                  const availableAssets = this.filterAvailableAssets(selectedNativeTokens, assets);
                  this.addNewAssetRow(
                    id,
                    availableAssets[0].fingerprint,
                    availableAssets[0]
                  );
                }}
                skin={ButtonSkin}
              />
            </div>
          </>
        )}
      </div>
    )
  };

  removeAssetRow = (name: string, assetIndex: number) => {
    const { sendFormFields } = this.state;
    const { receiver } = sendFormFields;
    const assets = receiver.formAssets;
    let fingerprint = '';
    let id = '';
    if (assets[assetIndex]) {
      const splitedName = name.split('_');
      if (splitedName && splitedName[1] && splitedName[2]) {
        fingerprint = splitedName[1];
        id = splitedName[2];
      }
      this.clearAssetValue(assets[assetIndex]);
      this.removeFormField(fingerprint, id);
    }
    if (fingerprint) {
      sendFormFields.receiver.formAssets = sendFormFields.receiver.formAssets.filter((item) => item.fingerprint === fingerprint);
      this.setState({
        sendFormFields,
      });
    }
  };

  removeSendFormField = () => {
    const { sendFormFields } = this.state;
    const { receiver } = sendFormFields;
    if (receiver) {
      delete sendFormFields.receiver;
      this.setState({
        sendFormFields,
      });
    }
  };

  removeFormField = (fingerprint: string, id: string) => {
    const assetFieldToDelete = `asset_${fingerprint}_${id}`;
    this.form.del(assetFieldToDelete);
    const walletsDropdownFieldToDelete = `walletsDropdown_${fingerprint}_${id}`;
    this.form.del(walletsDropdownFieldToDelete);
  };

  updateAssetsStateFormFields = (
    index: number,
    assetIndex: number
  ) => {
    const { sendFormFields } = this.state;
    const { receiver } = sendFormFields;
    if (receiver) {
      const receiversAssets = receiver.formAssets;
      if (receiversAssets) {
        sendFormFields.receiver.formAssets.splice(assetIndex, 1);
        sendFormFields.receiver.walletsDropdown.splice(assetIndex, 1);
        sendFormFields.receiver.selectedNativeTokens.splice(assetIndex, 1);
        this.setState({
          sendFormFields,
        });
      }
    }
  };

  addNewAssetField = (fingerprint: string, id: number) => {
    const newAsset = `asset_${fingerprint}_${id}`;
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
        const adaAssetField = form.$('adaField');
        const adaAssetFieldValue = adaAssetField.value;
        const isAdaAssetFieldValueValid = adaAssetField.isValid;
        const { sendFormFields } = this.state;
        const { receiver } = sendFormFields;
        let selectedNativeTokens;
        if (receiver) {
          selectedNativeTokens = receiver.selectedNativeTokens;
        }
        let { assetsError } = this.state;
        const selectedTokens = receiver.selectedNativeTokens;
        const selectedTokenId = selectedTokens && selectedTokens.length
          ? selectedTokens.length - 1
          : 0;
        const selectedToken = selectedTokens[selectedTokenId];
        let isAmountLessThenMax = true;
        if (selectedToken) {
          const selectedTokenValue = selectedToken.quantity;
          isAmountLessThenMax = Number(field.value) <= selectedTokenValue;
        }
        if (
          isValid &&
          isAmountLessThenMax &&
          isReceiverValid &&
          isAdaAssetFieldValueValid
        ) {
          this._isCalculatingAssetsFee = false;
          let assets = [];
          if (selectedNativeTokens && selectedNativeTokens.length) {
            assets = selectedNativeTokens.map((item) => {
              return {
                policy_id: item.policyId,
                asset_name: item.assetName,
                quantity: item.quantity.toNumber(),
              };
            });
          }
          this.calculateTransactionFee(
            receiverValue,
            adaAssetFieldValue,
            assets
          );
        } else if (!isAmountLessThenMax) {
          const error = this.context.intl.formatMessage(messages.invalidAmount);
          if (!assetsError) {
            assetsError = [];
          }
          assetsError[selectedTokenId] = error;
          this._isCalculatingTransactionFee = false;
          this._isCalculatingAssetsFee = false;
          this.setState({
            isTransactionFeeCalculated: false,
            transactionFee: new BigNumber(0),
            assetsError,
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
  };

  addNewWalletsDropdownField = (fingerprint: string, id: number) => {
    const newWalletsDropdown = `walletsDropdown_${fingerprint}_${id}`;
    this.form.add({
      name: newWalletsDropdown,
      value: null,
      key: newWalletsDropdown,
    });
    this.form.$(newWalletsDropdown).set('type', 'select');
  };

  addNewAssetRow = (
    index: number,
    fingerprint: string,
    selectedNativeToken: Asset,
  ) => {
    this.addNewAssetField(fingerprint, index);
    this.addNewWalletsDropdownField(fingerprint, index);
    this.setFormFields(
      false,
      fingerprint,
      index,
      selectedNativeToken
    );
    this.onSelectAsset(index - 1, selectedNativeToken.fingerprint);
  };

  onSelectAsset = (index: number, fingerprint: string) => {
    const { selectedAssetFingerprints } = this.state;
    if (!selectedAssetFingerprints.length) {
      selectedAssetFingerprints[0] = fingerprint;
    } else {
      selectedAssetFingerprints[index] = fingerprint;
    }
    this.setState({
      selectedAssetFingerprints,
    });
  };

  updateSelectedNativeTokens = (
    fingerprint: string,
    index: number,
  ) => {
    const { sendFormFields } = this.state;
    const allSelectedNativeToken =
      sendFormFields &&
      sendFormFields.receiver &&
      sendFormFields.receiver.selectedNativeTokens
        ? sendFormFields.receiver.selectedNativeTokens
        :[];
    const currentNativeToken = this.getNativeTokenById(fingerprint);
    allSelectedNativeToken[index] = currentNativeToken;
    this.setState((prevState) => ({
      sendFormFields: {
        ...prevState.sendFormFields,
        receiver: {
          ...prevState.sendFormFields.receiver,
          selectedNativeTokens: allSelectedNativeToken,
        },
      },
    }));
  };

  getNativeTokenById = (
    selectedAssetFingerprint: string
  ): ?WalletSummaryAsset => {
    const { assets } = this.props;
    return assets.find(
      (asset) => asset.fingerprint === selectedAssetFingerprint
    );
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
      currencyUnit,
      currencyMaxFractionalDigits,
      isDialogOpen,
      isRestoreActive,
      onExternalLinkClick,
      hwDeviceStatus,
      isHardwareWallet,
      assets,
    } = this.props;

    const {
      isTransactionFeeCalculated,
      transactionFee,
      transactionFeeError,
      selectedAssetFingerprints,
      isResetButtonDisabled,
      sendFormFields,
    } = this.state;

    const receiverField = form.$('receiver');
    const adaAssetField = form.$('adaField');
    const receiverFieldProps = receiverField.bind();
    const adaAssetFieldProps = adaAssetField.bind();
    const amount = new BigNumber(adaAssetFieldProps.value || 0);

    let fees = null;
    let total = null;
    if (isTransactionFeeCalculated) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = amount.plus(transactionFee).toFormat(currencyMaxFractionalDigits);
    }

    const selectedNativeTokenItem =
      selectedAssetFingerprints &&
      selectedAssetFingerprints.length &&
      assets &&
      assets.length
        ? this.getNativeTokenById(selectedAssetFingerprints[0])
        : null;

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
              {Object.keys(sendFormFields).map(
                (id: string, index: number) => (
                  <Fragment key={id}>
                    {this.renderReceiverRow(index)}
                  </Fragment>
                )
              )}
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
                  onClick={() => this.handleOnReset()}
                  skin={ButtonSkin}
                  disabled={isResetButtonDisabled}
                />
                <Button
                  className="primary"
                  label={intl.formatMessage(messages.sendButtonLabel)}
                  onClick={this.handleOnSubmit}
                  skin={ButtonSkin}
                  disabled={this.isDisabled()}
                />
              </div>
            </div>
          </BorderedBox>
        )}

        {isDialogOpen(WalletAssetsSendConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
            assets={this.transactionAssets}
            assetsAmounts={this.transactionAssetsAmounts}
            amount={amount.toFormat(currencyMaxFractionalDigits)}
            receiver={receiverFieldProps.value}
            multipleReceivers={[
              receiverFieldProps.value,
              receiverFieldProps.value,
            ]}
            totalAmount={total}
            transactionFee={fees}
            amountToNaturalUnits={formattedAmountToNaturalUnits}
            currencyUnit={
              selectedNativeTokenItem && selectedNativeTokenItem.metadata
                ? selectedNativeTokenItem.metadata.acronym
                : currencyUnit
            }
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
          />
        ) : null}
      </div>
    );
  }
}

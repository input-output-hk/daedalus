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
import infoIconInline from '../../assets/images/info-icon.inline.svg';
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
  addNewReceiverButtonLabel: {
    id: 'wallet.send.form.button.addNewReceiver',
    defaultMessage: '!!!+ Add another receiver',
    description:
      'Label for the "+ Add another receiver" button in the wallet send form.',
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
    defaultMessage: '!!!a minimum of {adaValue} ADA required',
    description:
      'Label for the min ADA required value in the wallet send form.',
  },
  minAdaRequiredTooltip: {
    id: 'wallet.send.form.minAdaRequiredTooltip',
    defaultMessage:
      '!!!A minimum of {adaValue} ADA needs to be sent to this receiver since you are sending other assets.',
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
    assets: AssetItems,
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
  assets: Array<WalletSummaryAsset>,
  isClearTooltipOpeningDownward?: boolean,
  hasAssets: boolean,
  selectedToken?: Asset,
};

type State = {
  isTransactionFeeCalculated: boolean,
  transactionFee: BigNumber,
  feeCalculationRequestQue: number,
  transactionFeeError: ?string | ?Node,
  assetsError: ?Array<?string | ?Node>,
  showReceiverRemoveBtn: boolean,
  showAssetRemoveBtn: Array<boolean>,
  sendFormFields: Object,
  selectedAssetFingerprints: Array<string>,
  showReceiverField: Array<boolean>,
  isResetButtonDisabled: boolean,
  filteredAssets: any,
  minimumAda: BigNumber,
  isReceiverAddressValid: boolean,
};

@observer
export default class WalletAssetsSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isTransactionFeeCalculated: false,
    transactionFee: new BigNumber(0),
    feeCalculationRequestQue: 0,
    transactionFeeError: null,
    assetsError: null,
    showReceiverRemoveBtn: false,
    showAssetRemoveBtn: [],
    sendFormFields: {},
    selectedAssetFingerprints: [],
    showReceiverField: [],
    isResetButtonDisabled: true,
    filteredAssets: [],
    minimumAda: new BigNumber(0),
    isReceiverAddressValid: false,
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
    this.setFormFields(false, 1, 'receiver1');
    this.filterAssets(assets, true);
    if (selectedToken) {
      setTimeout(() => {
        const { sendFormFields } = this.state;
        const { receiver1 } = sendFormFields;
        const { selectedNativeTokens, asset, walletsDropdown } = receiver1;
        const id = selectedNativeTokens.length;
        const newFilteredAssets = this.filterAssets(
          assets,
          id === 0,
          id > 0 ? selectedNativeTokens : null
        );
        this.addNewAssetRow(
          1,
          `asset${asset.length + 1}`,
          'receiver1',
          `walletsDropdown${walletsDropdown.length + 1}`,
          newFilteredAssets[id][0]
        );
      }, 2000);
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  filterAssets = (
    allAssets: Array<WalletSummaryAsset>,
    firstTimeFiltering?: boolean,
    assetsToRemove?: any
  ): Array<WalletSummaryAsset> => {
    const { filteredAssets } = this.state;
    const { assets } = this.props;
    const currentFilteredAssets: any = firstTimeFiltering
      ? [assets]
      : filteredAssets;
    let newFilteredAssets = [];
    if (
      !firstTimeFiltering &&
      currentFilteredAssets &&
      currentFilteredAssets.length
    ) {
      currentFilteredAssets[currentFilteredAssets.length] = assets;
    }
    if (assetsToRemove && assetsToRemove.length) {
      // eslint-disable-next-line array-callback-return
      assetsToRemove.map((item) => {
        newFilteredAssets.push(
          ...currentFilteredAssets[currentFilteredAssets.length - 1].filter(
            (asset) => asset.fingerprint !== item.fingerprint
          )
        );
      });
    }
    if (firstTimeFiltering) {
      newFilteredAssets = currentFilteredAssets;
    } else {
      currentFilteredAssets[
        currentFilteredAssets.length - 1
      ] = newFilteredAssets;
      if (
        currentFilteredAssets[currentFilteredAssets.length - 2] &&
        currentFilteredAssets[currentFilteredAssets.length - 2].length
      ) {
        const newAssets = newFilteredAssets.map((item) => {
          return currentFilteredAssets[currentFilteredAssets.length - 2].filter(
            (asset) => asset.fingerprint !== item.fingerprint
          );
        });
        currentFilteredAssets[currentFilteredAssets.length - 2] = Object.values(
          ...newAssets
        );
      }
      newFilteredAssets = currentFilteredAssets;
    }
    const selectedAssets =
      newFilteredAssets && newFilteredAssets.length
        ? newFilteredAssets
        : currentFilteredAssets;
    this.setState({
      filteredAssets: selectedAssets,
    });
    return selectedAssets;
  };

  get transactionAssetsAmounts() {
    const { sendFormFields } = this.state;
    const assetsFields = get(sendFormFields, 'receiver1.asset');
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

  clearReceiverAddress = (index?: number) => {
    const receiverField = this.form.$(index ? `receiver${index}` : 'receiver1');
    if (receiverField) {
      receiverField.clear();
      this.setReceiverValidity(true);
    }
  };

  clearAssetValue = (singleAsset?: any) => {
    const assetField = singleAsset || this.form.$('receiver1_asset1');
    if (assetField) {
      assetField.clear();
    }
  };

  handleOnReset = (receiverId: string) => {
    this.form.reset();
    this.disableResetButton();
    this.hideReceiverField();
    const { sendFormFields } = this.state;
    const receiverFields = sendFormFields[receiverId];
    if (receiverFields) {
      receiverFields.asset.map((singleAsset) =>
        this.clearAssetValue(singleAsset)
      );
    }
    this.clearReceiverAddress();
    this.setFormFields(true, 1, 'receiver1');
  };

  disableResetButton = () => {
    this.setState({
      isResetButtonDisabled: true,
    });
  };

  setFormFields = (
    resetFormFields: boolean,
    id: number,
    receiverId: string,
    assetId?: string,
    dropdownId?: string,
    selectedNativeToken?: Asset
  ) => {
    const formFields = this.form.fields;
    const receiverField = formFields.get(`receiver${id}`);
    const assetAdaField = formFields.get(`${receiverId}_adaAsset`);
    const assetField = assetId
      ? formFields.get(`${receiverId}_${assetId}`)
      : null;
    const walletsDropdownField = dropdownId
      ? formFields.get(`${receiverId}_${dropdownId}`)
      : null;
    if (resetFormFields) {
      this.setState({
        sendFormFields: {
          [receiverId]: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            asset: [],
            walletsDropdown: [],
            selectedNativeTokens: [],
          },
        },
        filteredAssets: [],
      });
      const { assets } = this.props;
      this.filterAssets(assets, true);
    } else {
      const { sendFormFields } = this.state;
      const currentReceiverFields = sendFormFields[receiverId];
      let currentAssets = [];
      let currentWalletsDropdown = [];
      let currentSelectedNativeTokens = [];
      if (currentReceiverFields) {
        currentAssets = currentReceiverFields.asset;
        currentWalletsDropdown = currentReceiverFields.walletsDropdown;
        currentSelectedNativeTokens =
          currentReceiverFields.selectedNativeTokens;
      }
      if (assetField) {
        currentAssets.push(assetField);
      }
      if (walletsDropdownField) {
        currentWalletsDropdown.push(walletsDropdownField);
      }
      if (selectedNativeToken) {
        currentSelectedNativeTokens.push(selectedNativeToken);
      }

      this.setState((prevState) => ({
        sendFormFields: {
          ...prevState.sendFormFields,
          [receiverId]: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            asset: currentAssets,
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
        receiver1: {
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
        receiver1_adaAsset: {
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
              const receiverField = form.$('receiver1');
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
        receiver1_asset1: {
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
              const receiverField = form.$('receiver1');
              const receiverValue = receiverField.value;
              const isReceiverValid = receiverField.isValid;
              const { sendFormFields } = this.state;
              const { receiver1 } = sendFormFields;
              let selectedNativeTokens;
              if (receiver1) {
                selectedNativeTokens = receiver1.selectedNativeTokens;
              }
              if (isValid && isReceiverValid) {
                let assets = [];
                if (selectedNativeTokens.length) {
                  assets = selectedNativeTokens.map(item => {
                    return {
                      policy_id: item.policyId,
                      asset_name: item.assetName,
                      quantity: item.quantity.toNumber(),
                    };
                  });
                }
                this.calculateTransactionFee(receiverValue, amountValue, assets);
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
        receiver1_walletsDropdown1: {
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

  calculateTransactionFee = async (address: string, amountValue: string, assets?: AssetItems) => {
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
        const transactionFeeError = errorHasLink ? (
          <FormattedHTMLMessageWithLink
            message={error}
            onExternalLinkClick={this.props.onExternalLinkClick}
          />
        ) : (
          this.context.intl.formatMessage(error)
        );
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

  hideReceiverField = (index?: number) => {
    if (index) {
      if (index > 1) {
        const receiverToDelete = `receiver${index}`;
        this.form.del(receiverToDelete);
      }
      const { showReceiverField } = this.state;
      const filteredReceiverFields = showReceiverField;
      filteredReceiverFields[index - 1] = false;
      this.setState({
        showReceiverField: filteredReceiverFields,
      });
    } else {
      this.setState({
        showReceiverField: [],
      });
    }
  };

  hasReceiverValue = (index: number) => {
    const receiverField = this.form.$(`receiver${index}`);
    return receiverField.value.length > 0;
  };

  hasAssetValue = (asset: any) => {
    return asset && asset.value;
  };

  showRemoveButton = () => {
    this.setState({
      showReceiverRemoveBtn: true,
    });
  };

  hideRemoveButton = () => {
    this.setState({
      showReceiverRemoveBtn: false,
    });
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

  renderReceiverRow = (receiverId: string, index: number): Node => {
    const { intl } = this.context;
    const {
      isClearTooltipOpeningDownward,
      currencyMaxFractionalDigits,
      walletAmount,
      isHardwareWallet,
      assets,
    } = this.props;

    const {
      showReceiverField,
      showReceiverRemoveBtn,
      selectedAssetFingerprints,
      isTransactionFeeCalculated,
      transactionFee,
      transactionFeeError,
      assetsError,
      sendFormFields,
      filteredAssets,
      minimumAda,
      isReceiverAddressValid,
    } = this.state;

    const {
      receiver,
      adaAsset,
      asset,
      walletsDropdown,
      selectedNativeTokens,
    } = sendFormFields[receiverId];

    const receiverField = receiver;

    const walletsDropdownFieldProps = walletsDropdown.map((dropdown) =>
      dropdown.bind()
    );
    const adaAssetFieldProps = adaAsset.bind();

    const assetFieldProps = asset.map((singleAsset) => singleAsset.bind());

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

    const removeReceiverButtonClasses = classNames([
      styles.removeReceiverButton,
      'flat',
      showReceiverRemoveBtn ? styles.active : null,
    ]);

    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight =
      asset && asset.length
        ? assetsSeparatorBasicHeight * (asset.length + 1) - 40 * asset.length
        : assetsSeparatorBasicHeight;

    const tokenDecimalPlaces = 0;

    const sortedAssets = filteredAssets.map((item) =>
      orderBy(item, 'metadata.acronym', 'asc')
    );

    let minimumAdaValue = TRANSACTION_MIN_ADA_VALUE;

    if (minimumAda && !minimumAda.isZero()) {
      minimumAdaValue = minimumAda.toFormat();
    }

    if (transactionFee && !transactionFee.isZero()) {
      minimumAdaValue = transactionFee.toFormat();
    }

    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      !filteredAssets.length ||
      filteredAssets[filteredAssets.length - 1].length === 1
        ? styles.disabled
        : null,
      'primary',
    ]);

    return (showReceiverField && index > 0 && showReceiverField[index]) ||
      index === 0 ? (
      <div className={styles.fieldsContainer}>
        <div
          onMouseEnter={this.showRemoveButton}
          onMouseLeave={this.hideRemoveButton}
          className={styles.receiverInput}
        >
          {showReceiverField && showReceiverField[index] && (
            <Button
              className={removeReceiverButtonClasses}
              label={intl.formatMessage(messages.removeReceiverButtonLabel)}
              onClick={() => this.removeReceiverRow(index + 1, receiverId)}
              skin={ButtonSkin}
            />
          )}
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
          {this.hasReceiverValue(index + 1) && (
            <div className={styles.clearReceiverContainer}>
              <PopOver
                content="Clear"
                placement={isClearTooltipOpeningDownward ? 'bottom' : 'top'}
              >
                <button
                  onClick={() => this.handleOnReset('receiver1')}
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
        {this.hasReceiverValue(index + 1) && isReceiverAddressValid && (
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
                      adaValue: minimumAdaValue,
                    })}
                  </span>
                  <PopOver
                    content={intl.formatMessage(
                      messages.minAdaRequiredTooltip,
                      {
                        adaValue: minimumAdaValue,
                      }
                    )}
                    contentClassName={styles.minAdaTooltipContent}
                    key="tooltip"
                  >
                    <SVGInline
                      svg={infoIconInline}
                      className={styles.infoIcon}
                    />
                  </PopOver>
                </div>
              </Fragment>
              <Fragment>
                {asset.map((singleAsset: any, assetIndex: number) => {
                  const token = selectedNativeTokens[assetIndex] || {};
                  const { quantity, metadata } = token;
                  return (
                    <div // eslint-disable-next-line react/no-array-index-key
                      key={`${receiverId}_asset${assetIndex}`}
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
                          this.removeAssetRow(index + 1, receiverId, assetIndex)
                        }
                        skin={ButtonSkin}
                      />
                      <NumericInput
                        {...assetFieldProps[assetIndex]}
                        placeholder={
                          metadata
                            ? `0${
                                this.getCurrentNumberFormat().decimalSeparator
                              }${'0'.repeat(
                                this.props.currencyMaxFractionalDigits
                              )}`
                            : '0'
                        }
                        className={classNames([
                          styles.assetItem,
                          this.state.showAssetRemoveBtn &&
                          this.state.showAssetRemoveBtn[assetIndex]
                            ? styles.hasButton
                            : null,
                        ])}
                        label={`${intl.formatMessage(messages.assetLabel)} #${
                          assetIndex + 1
                        }`}
                        bigNumberFormat={this.getCurrentNumberFormat()}
                        decimalPlaces={tokenDecimalPlaces}
                        numberLocaleOptions={{
                          minimumFractionDigits: tokenDecimalPlaces,
                        }}
                        onChange={(value) => {
                          this._isCalculatingAssetsFee = true;
                          this.setState({
                            isResetButtonDisabled: false,
                          });
                          asset[assetIndex].onChange(value);
                          estimatedField.onChange(fees);
                        }}
                        currency={metadata ? metadata.acronym : null}
                        value={amount[assetIndex]}
                        error={
                          asset.error ||
                          (assetsError && assetsError[assetIndex]
                            ? assetsError[assetIndex]
                            : null)
                        }
                        skin={AmountInputSkin}
                        onKeyPress={this.handleSubmitOnEnter}
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
                            <div className={styles.separator} />
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
                            assets={sortedAssets[assetIndex]}
                            onChange={(id) => {
                              this.onSelectAsset(index, id);
                              this.updateSelectedNativeTokens(
                                id,
                                assetIndex,
                                receiverId
                              );
                              const focusedField = this.form.$(
                                `${receiverId}_asset${assetIndex + 1}`
                              );
                              if (focusedField) {
                                focusedField.clear();
                              }
                            }}
                            syncingLabel={intl.formatMessage(
                              messages.syncingWallet
                            )}
                            hasAssetsEnabled
                            value={selectedAssetFingerprints[assetIndex]}
                            getStakePoolById={() => {}}
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
                  const id = selectedNativeTokens.length;
                  const newFilteredAssets = this.filterAssets(
                    assets,
                    id === 0,
                    id > 0 ? selectedNativeTokens : null
                  );
                  this.addNewAssetRow(
                    index + 1,
                    `asset${asset.length + 1}`,
                    receiverId,
                    `walletsDropdown${walletsDropdown.length + 1}`,
                    newFilteredAssets[id][0]
                  );
                }}
                skin={ButtonSkin}
              />
            </div>
          </>
        )}
      </div>
    ) : null;
  };

  removeReceiverRow = (index: number, receiverId: string) => {
    const { sendFormFields } = this.state;
    const receiver = sendFormFields[receiverId];
    const assets = receiver.asset;
    assets.map((singleAsset) => this.clearAssetValue(singleAsset));
    this.clearReceiverAddress(index);
    this.hideReceiverField(index);
    if (index > 1) {
      this.removeFormField(index, receiverId);
      this.removeSendFormField(receiverId);
    } else {
      this.disableResetButton();
    }
  };

  removeAssetRow = (index: number, receiverId: string, assetIndex: number) => {
    const { sendFormFields, filteredAssets } = this.state;
    const receiver = sendFormFields[receiverId];
    const assets = receiver.asset;
    if (assets[assetIndex]) {
      this.clearAssetValue(assets[assetIndex]);
      this.removeFormField(assetIndex + 1, receiverId);
      this.updateAssetsStateFormFields(index, receiverId, assetIndex);
      if (filteredAssets && filteredAssets[assetIndex]) {
        const currentFilterAssetValue = filteredAssets[assetIndex];
        const newFilteredAssets = filteredAssets.filter(
          (item, id) => id !== assetIndex
        );
        newFilteredAssets.map((item) => item.push(...currentFilterAssetValue));
        this.setState({
          filteredAssets: newFilteredAssets,
        });
      }
    }
  };

  removeSendFormField = (receiverId: string) => {
    const { sendFormFields } = this.state;
    if (sendFormFields && sendFormFields[receiverId]) {
      delete sendFormFields[receiverId];
      this.setState({
        sendFormFields,
      });
    }
  };

  removeFormField = (index: number, receiverId: string) => {
    const assetFieldToDelete = `${receiverId}_asset${index}`;
    this.form.del(assetFieldToDelete);
    const walletsDropdownFieldToDelete = `${receiverId}_walletsDropdown${index}`;
    this.form.del(walletsDropdownFieldToDelete);
  };

  updateAssetsStateFormFields = (
    index: number,
    receiverId: string,
    assetIndex: number
  ) => {
    const { sendFormFields } = this.state;
    const receiverFields = sendFormFields[receiverId];
    if (receiverFields) {
      const receiversAssets = receiverFields.asset;
      if (receiversAssets) {
        sendFormFields[receiverId].asset.splice(assetIndex, 1);
        sendFormFields[receiverId].walletsDropdown.splice(assetIndex, 1);
        sendFormFields[receiverId].selectedNativeTokens.splice(assetIndex, 1);
        this.setState({
          sendFormFields,
        });
      }
    }
  };

  addNewAssetField = (receiverId: string, assetId: string) => {
    const newAsset = `${receiverId}_${assetId}`;
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
        const receiverField = form.$(receiverId);
        const receiverValue = receiverField.value;
        const isReceiverValid = receiverField.isValid;
        const { sendFormFields } = this.state;
        const { receiver1 } = sendFormFields;
        let selectedNativeTokens;
        if (receiver1) {
          selectedNativeTokens = receiver1.selectedNativeTokens;
        }
        let { assetsError } = this.state;
        const receiver = sendFormFields[receiverId];
        const selectedTokens = receiver.selectedNativeTokens;
        const selectedTokenId = selectedTokens.length
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
          isReceiverValid
        ) {
          this._isCalculatingAssetsFee = false;
          let assets = [];
          if (selectedNativeTokens && selectedNativeTokens.length) {
            assets = selectedNativeTokens.map(item => {
              return {
                policy_id: item.policyId,
                asset_name: item.assetName,
                quantity: item.quantity.toNumber(),
              };
            });
          }
          this.calculateTransactionFee(receiverValue, field.value, assets);
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
  };

  addNewWalletsDropdownField = (receiverId: string, dropdownId: string) => {
    const newWalletsDropdown = `${receiverId}_${dropdownId}`;
    this.form.add({
      name: newWalletsDropdown,
      value: null,
      key: newWalletsDropdown,
    });
    this.form.$(newWalletsDropdown).set('type', 'select');
  };

  addNewAssetRow = (
    index: number,
    assetId: string,
    receiverId: string,
    dropdownId: string,
    selectedNativeToken: Asset
  ) => {
    this.addNewAssetField(receiverId, assetId);
    this.addNewWalletsDropdownField(receiverId, dropdownId);
    this.setFormFields(
      false,
      index,
      receiverId,
      assetId,
      dropdownId,
      selectedNativeToken
    );
    const splitedAssetId = assetId.split('asset');
    const assetIndex = splitedAssetId ? parseInt(splitedAssetId[1], 10) : 0;
    this.onSelectAsset(assetIndex - 1, selectedNativeToken.fingerprint);
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
    id: string,
    index: number,
    receiverId: string
  ) => {
    const { sendFormFields } = this.state;
    const allSelectedNativeToken =
      sendFormFields[receiverId].selectedNativeTokens;
    const currentNativeToken = this.getNativeTokenById(id);
    allSelectedNativeToken[index] = currentNativeToken;
    this.setState((prevState) => ({
      sendFormFields: {
        ...prevState.sendFormFields,
        [receiverId]: {
          ...prevState.sendFormFields[receiverId],
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

    const receiverField = form.$('receiver1');
    const adaAssetField = form.$('receiver1_adaAsset');
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
                (receiverId: string, index: number) => (
                  <Fragment key={receiverId}>
                    {this.renderReceiverRow(receiverId, index)}
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
                  onClick={() => this.handleOnReset('receiver1')}
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

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
import { get, orderBy } from 'lodash';
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
import type { WalletSummaryAsset } from '../../api/assets/types';
import infoIconInline from '../../assets/images/info-icon.inline.svg';

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
    defaultMessage: '!!!+ Add another token',
    description:
      'Label for the "+ Add another token" button in the wallet send form.',
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
  currencyMaxIntegerDigits?: number,
  currencyMaxFractionalDigits: number,
  validateAmount: (amountInNaturalUnits: string) => Promise<boolean>,
  calculateTransactionFee: (
    address: string,
    amount: number
  ) => Promise<BigNumber>,
  currentNumberFormat: string,
  addressValidator: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
  onExternalLinkClick?: Function,
  isRestoreActive: boolean,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
  assets: Array<WalletSummaryAsset>,
  isClearTooltipOpeningDownward?: boolean,
  selectedNativeToken: ?Asset,
  walletAmount: BigNumber,
};

type State = {
  isTransactionFeeCalculated: boolean,
  transactionFee: BigNumber,
  feeCalculationRequestQue: number,
  transactionFeeError: ?string | ?Node,
  showReceiverRemoveBtn: boolean,
  sendFormFields: Object,
  selectedAssetId: ?string,
  showReceiverField: Array<boolean>,
  isResetButtonDisabled: boolean,
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
    showReceiverRemoveBtn: false,
    sendFormFields: {},
    selectedAssetId: null,
    showReceiverField: [],
    isResetButtonDisabled: true,
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
    const { selectedNativeToken } = this.props;
    if (selectedNativeToken) {
      this.onSelectAsset(selectedNativeToken.fingerprint, 0, 'receiver1');
    }
    this.setFormFields(false, 1, 'receiver1');
  }

  componentWillUnmount() {
    this._isMounted = false;
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
    }
  };

  clearAssetValue = (singleAsset?: any) => {
    const assetField = singleAsset || this.form.$('receiver1_asset1');
    if (assetField) {
      assetField.clear();
    }
  };

  clearAdaAssetValue = (index?: number) => {
    const receiverField = this.form.$(
      index ? `receiver${index}_adaAsset` : 'receiver1__adaAsset'
    );
    if (receiverField) {
      receiverField.clear();
    }
  };

  handleOnReset = () => {
    this.form.reset();
    this.disableResetButton();
    this.hideReceiverField();
    this.clearAssetValue();
    this.clearReceiverAddress();
    this.setFormFields(
      true,
      1,
      'receiver1',
      'receiver1_asset1',
      'receiver1_walletsDropdown1'
    );
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
    dropdownId?: string
  ) => {
    const formFields = this.form.fields;
    const receiverField = formFields.get(`receiver${id}`);
    const assetAdaField = formFields.get(`${receiverId}_adaAsset`);
    const assetField = assetId ? formFields.get(`${receiverId}_${assetId}`) : null;
    const walletsDropdownField = dropdownId ? formFields.get(`${receiverId}_${dropdownId}`) : null;
    const { selectedNativeToken } = this.props;
    if (resetFormFields) {
      this.setState({
        sendFormFields: {
          [receiverId]: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            asset: [],
            walletsDropdown: [],
            selectedNativeToken,
          },
        },
      });
    } else {
      const { sendFormFields } = this.state;
      const currentReceiverFields = sendFormFields[receiverId];
      let currentAssets = [];
      let currentWalletsDropdown = [];
      if (currentReceiverFields) {
        currentAssets = currentReceiverFields.asset;
        currentWalletsDropdown = currentReceiverFields.walletsDropdown;
      }
      if (assetField) {
        currentAssets.push(assetField);
      }
      if (walletsDropdownField) {
        currentWalletsDropdown.push(walletsDropdownField);
      }
      this.setState((prevState) => ({
        sendFormFields: {
          ...prevState.sendFormFields,
          [receiverId]: {
            receiver: receiverField,
            adaAsset: assetAdaField,
            asset: currentAssets,
            walletsDropdown: currentWalletsDropdown,
            selectedNativeToken,
          },
        },
      }));
    }
  };

  updateFormFields = (id: string, assetId: string, receiverId: string) => {
    const { assets } = this.props;
    const { sendFormFields } = this.state;
    if (sendFormFields && sendFormFields[receiverId]) {
      const selectedNativeTokenItem =
        assetId && assets && assets.length
          ? this.getNativeTokenById(assetId)
          : null;
      const sendFormFieldItem = sendFormFields[receiverId];
      sendFormFieldItem.selectedNativeToken = selectedNativeTokenItem;
      sendFormFields[receiverId] = sendFormFieldItem;
      this.setState({
        sendFormFields,
      });
    }
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.handleOnSubmit);

  isDisabled = () =>
    this._isCalculatingTransactionFee || !this.state.isTransactionFeeCalculated;

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
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              const isValidAddress = await this.props.addressValidator(value);
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
          label: `${this.context.intl.formatMessage(messages.assetLabel)} #1`,
          placeholder: `0${
            this.getCurrentNumberFormat().decimalSeparator
          }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
          value: '1',
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
          placeholder: `0${
            this.getCurrentNumberFormat().decimalSeparator
          }${'0'.repeat(this.props.currencyMaxFractionalDigits)}`,
          value: null,
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
      this.setState({
        isTransactionFeeCalculated: false,
        transactionFee: new BigNumber(0),
        transactionFeeError: null,
      });
    }
  }

  isLatestTransactionFeeRequest = (
    currentFeeCalculationRequestQue: number,
    prevFeeCalculationRequestQue: number
  ) => currentFeeCalculationRequestQue - prevFeeCalculationRequestQue === 1;

  calculateTransactionFee = async (address: string, amountValue: string) => {
    const amount = formattedAmountToLovelace(amountValue);
    const {
      feeCalculationRequestQue: prevFeeCalculationRequestQue,
    } = this.state;
    this.setState((prevState) => ({
      isTransactionFeeCalculated: false,
      transactionFee: new BigNumber(0),
      transactionFeeError: null,
      feeCalculationRequestQue: prevState.feeCalculationRequestQue + 1,
    }));
    try {
      const fee = await this.props.calculateTransactionFee(address, amount);
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

  showReceiverField = (index: number) => {
    const { showReceiverField } = this.state;
    showReceiverField[index] = true;
    this.setState({
      showReceiverField,
    });
  };

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

  get isReceiverValid() {
    const receiverField = this.form.$('receiver1');
    return receiverField.isValid;
  }

  hasAssetValue = (receiverId: string, assetId: string) => {
    const assetField = this.form.$(`${receiverId}_asset${assetId}`);
    return !!assetField.value;
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

  renderReceiverRow = (receiverId: string, index: number): Node => {
    const { intl } = this.context;
    const {
      isClearTooltipOpeningDownward,
      currencyMaxFractionalDigits,
      assets,
      walletAmount,
    } = this.props;

    const {
      showReceiverField,
      showReceiverRemoveBtn,
      selectedAssetId,
      isTransactionFeeCalculated,
      transactionFee,
      sendFormFields,
    } = this.state;

    const {
      receiver,
      adaAsset,
      asset,
      walletsDropdown,
      selectedNativeToken,
    } = sendFormFields[receiverId];

    const receiverField = receiver;

    const walletsDropdownFieldProps = walletsDropdown.map((dropdown) =>
      dropdown.bind()
    );
    const adaAssetFieldProps = adaAsset.bind();

    const assetFieldProps = asset.map((singleAsset) => singleAsset.bind());

    const estimatedField = this.form.$('estimatedFee');

    const amount = assetFieldProps.map(
      (assetField) => new BigNumber(assetField.value || 0)
    );

    // const showReceiverLabelNumber = Object.keys(sendFormFields).length > 1;

    const receiverLabel = /* showReceiverLabelNumber
      ? `${intl.formatMessage(messages.receiverLabel)} #${index + 1}`
      : */ intl.formatMessage(
      messages.receiverLabel
    );

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

    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      'primary',
    ]);

    const assetsSeparatorBasicHeight = 140;
    const assetsSeparatorCalculatedHeight = asset && asset.length ?
      (assetsSeparatorBasicHeight * (asset.length + 1)) - (40 * asset.length) :
      assetsSeparatorBasicHeight;

    const tokenDecimalPlaces = 2;

    const sortedAssets = orderBy(assets, 'metadata.acronym', 'asc');

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
              receiverField.onChange(value || '');
              this.setState({
                isResetButtonDisabled: false,
              });
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
                  onClick={() => this.clearReceiverAddress(index + 1)}
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
        {this.hasReceiverValue(index + 1) && (
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
                  label={`${intl.formatMessage(messages.assetLabel)} #1`}
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
                  error={adaAsset.error}
                  onKeyPress={this.handleSubmitOnEnter}
                  allowSigns={false}
                />
                <div className={styles.minAdaRequired}>
                  <span>
                    {intl.formatMessage(messages.minAdaRequired, {
                      adaValue: 1,
                    })}
                  </span>
                  <PopOver
                    content={intl.formatMessage(
                      messages.minAdaRequiredTooltip,
                      {
                        adaValue: 1,
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
                {asset.map((singleAsset: any, assetIndex: number) => (
                  // eslint-disable-next-line react/no-array-index-key
                  <Fragment key={`${receiverId}_asset${assetIndex}`}>
                    {selectedNativeToken &&
                      selectedNativeToken.quantity &&
                      selectedNativeToken.metadata && (
                        <div className={styles.amountTokenTotal}>
                          {intl.formatMessage(messages.ofLabel)}&nbsp;
                          {formattedWalletAmount(
                            new BigNumber(selectedNativeToken.quantity),
                            false
                          )}
                          &nbsp;{selectedNativeToken.metadata.acronym}
                        </div>
                      )}
                    <NumericInput
                      {...assetFieldProps[assetIndex]}
                      className={styles.assetItem}
                      label={`${intl.formatMessage(messages.assetLabel)} #${
                        assetIndex + 2
                      }`}
                      bigNumberFormat={this.getCurrentNumberFormat()}
                      decimalPlaces={tokenDecimalPlaces}
                      numberLocaleOptions={{
                        minimumFractionDigits: tokenDecimalPlaces,
                      }}
                      onChange={(value) => {
                        this._isCalculatingTransactionFee = true;
                        this.setState({
                          isResetButtonDisabled: false,
                        });
                        asset[assetIndex].onChange(value);
                        estimatedField.onChange(fees);
                      }}
                      currency={
                        selectedNativeToken && selectedNativeToken.metadata
                          ? selectedNativeToken.metadata.acronym
                          : null
                      }
                      value={amount[assetIndex]}
                      error={asset.error}
                      skin={AmountInputSkin}
                      onKeyPress={this.handleSubmitOnEnter}
                      allowSigns={false}
                    />
                    {this.hasAssetValue(receiverId, assetIndex + 1) && (
                      <div className={styles.clearAssetContainer}>
                        <PopOver
                          content="Clear"
                          placement={
                            isClearTooltipOpeningDownward ? 'bottom' : 'top'
                          }
                        >
                          <button
                            onClick={() => this.clearAssetValue(singleAsset)}
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
                    <div className={styles.walletsDropdownWrapper}>
                      <WalletsDropdown
                        className={styles.walletsDropdown}
                        {...walletsDropdownFieldProps[index]}
                        numberOfStakePools={4}
                        assets={sortedAssets}
                        onChange={(id) =>
                          this.onSelectAsset(id, index, receiverId)
                        }
                        syncingLabel={intl.formatMessage(
                          messages.syncingWallet
                        )}
                        hasAssetsEnabled
                        value={selectedAssetId}
                        getStakePoolById={() => {}}
                        errorPosition="bottom"
                      />
                    </div>
                  </Fragment>
                ))}
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                onClick={() =>
                  this.addNewAssetRow(
                    index + 1,
                    `asset${asset.length + 1}`,
                    receiverId,
                    `walletsDropdown${walletsDropdown.length + 1}`
                  )
                }
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
      this.removeFormField(index);
      this.removeSendFormField(receiverId);
    } else {
      this.disableResetButton();
    }
  };

  removeSendFormField = (receiverId: number) => {
    const { sendFormFields } = this.state;
    if (sendFormFields && sendFormFields[receiverId]) {
      delete sendFormFields[receiverId];
      this.setState({
        sendFormFields,
      });
    }
  };

  removeFormField = (index: number) => {
    const assetFieldToDelete = `asset${index}`;
    this.form.del(assetFieldToDelete);
    const walletsDropdownFieldToDelete = `receiver1_walletsDropdown${index}`;
    this.form.del(walletsDropdownFieldToDelete);
  };

  addNewReceiverField = (receiverId: string) => {
    this.form.add({ name: receiverId, value: '', key: receiverId });
    this.form
      .$(receiverId)
      .set('label', this.context.intl.formatMessage(messages.receiverLabel));
    this.form
      .$(receiverId)
      .set(
        'placeholder',
        this.context.intl.formatMessage(messages.receiverHint)
      );
    this.form.$(receiverId).set('validators', [
      async ({ field }) => {
        const { value } = field;
        if (value === '') {
          this.resetTransactionFee();
          return [
            false,
            this.context.intl.formatMessage(messages.fieldIsRequired),
          ];
        }
        const isValidAddress = await this.props.addressValidator(value);
        return [
          isValidAddress,
          this.context.intl.formatMessage(apiErrorMessages.invalidAddress),
        ];
      },
    ]);
  };

  addNewAssetField = (receiverId: number, assetId: string) => {
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
    ]);
  };

  addNewWalletsDropdownField = (receiverId: string, dropdownId?: string) => {
    const newWalletsDropdown = `${receiverId}_${dropdownId}`;
    this.form.add({
      name: newWalletsDropdown,
      value: null,
      key: newWalletsDropdown,
    });
    this.form.$(newWalletsDropdown).set('type', 'select');
  };

  /* addNewReceiverRow = (
    index: number,
    receiverId: string,
    assetId: string,
    dropdownId: string
  ) => {
    this.addNewReceiverField(receiverId);
    this.addNewAssetField(receiverId, assetId);
    this.addNewWalletsDropdownField(receiverId, dropdownId);
    this.showReceiverField(index - 1);
    this.setFormFields(false, index, receiverId, assetId, dropdownId);
  }; */

  addNewAssetRow = (
    index: number,
    assetId: string,
    receiverId: string,
    dropdownId: string
  ) => {
    this.addNewAssetField(receiverId, assetId);
    this.addNewWalletsDropdownField(receiverId, dropdownId);
    this.setFormFields(false, index, receiverId, assetId, dropdownId);
  };

  onSelectAsset = (assetId: string, id: number, receiverId: string) => {
    this.setState({ selectedAssetId: assetId });
    this.updateFormFields(id, assetId, receiverId);
  };

  getNativeTokenById = (selectedAssetId: string): ?Asset => {
    const { assets } = this.props;
    return assets.find((asset) => asset.fingerprint === selectedAssetId);
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const {
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
      selectedAssetId,
      isResetButtonDisabled,
      sendFormFields,
    } = this.state;

    const assetField = form.$('receiver1_asset1');
    const receiverField = form.$('receiver1');
    const receiverFieldProps = receiverField.bind();
    const assetFieldProps = assetField.bind();
    const amount = new BigNumber(assetFieldProps.value || 0);

    let fees = null;
    let total = null;
    if (isTransactionFeeCalculated && transactionFee) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = amount.add(transactionFee).toFormat(currencyMaxFractionalDigits);
    }

    const selectedNativeTokenItem =
      selectedAssetId && assets && assets.length
        ? this.getNativeTokenById(selectedAssetId)
        : null;

    /* const newReceiverButtonClasses = classNames([
      styles.addNewReceiverButton,
      'flat',
    ]); */

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
              {/* Object.keys(sendFormFields).length > 0 && (
                <Button
                  className={newReceiverButtonClasses}
                  label={intl.formatMessage(messages.addNewReceiverButtonLabel)}
                  onClick={() =>
                    this.addNewReceiverRow(
                      Object.keys(sendFormFields).length + 1,
                      `receiver${Object.keys(sendFormFields).length + 1}`,
                      'asset1',
                      'walletsDropdown1'
                    )
                  }
                  skin={ButtonSkin}
                />
              ) */}
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
                  onClick={{}}
                  skin={ButtonSkin}
                  disabled={this.isDisabled()}
                />
              </div>
            </div>
          </BorderedBox>
        )}

        {isDialogOpen(WalletAssetsSendConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
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
                : null
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

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
import { get } from 'lodash';
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
      this.onSelectAsset(selectedNativeToken.id, 0, 'receiver1');
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

  clearAssetValue = (index?: number) => {
    const assetField = this.form.$(index ? `asset${index}` : 'asset1');
    if (assetField) {
      assetField.clear();
    }
  };

  handleOnReset = () => {
    this.form.reset();
    this.disableResetButton();
    this.hideReceiverField();
    this.clearAssetValue();
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
    receiverId: string
  ) => {
    const formFields = this.form.fields;
    const receiverField = formFields.get(`receiver${id}`);
    const assetField = formFields.get(`asset${id}`);
    const walletsDropdownField = formFields.get(`walletsDropdown${id}`);
    const { selectedNativeToken } = this.props;
    if (resetFormFields) {
      this.setState({
        sendFormFields: {
          [receiverId]: {
            receiver: receiverField,
            asset: assetField,
            walletsDropdown: walletsDropdownField,
            selectedNativeToken,
          },
        },
      });
    } else {
      this.setState((prevState) => ({
        sendFormFields: {
          ...prevState.sendFormFields,
          [receiverId]: {
            receiver: receiverField,
            asset: assetField,
            walletsDropdown: walletsDropdownField,
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

  receiverFieldRef: Array<Input> = [];

  assetFieldRef: Array<NumericInput> = [];

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
            async ({ field, form }) => {
              const { value } = field;
              if (value === '') {
                this.resetTransactionFee();
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              const assetField = form.$('asset1');
              const isAmountValid = assetField.isValid;
              const isValidAddress = await this.props.addressValidator(value);
              if (isValidAddress && isAmountValid) {
                const amountValue = assetField.value.toString();
                this.calculateTransactionFee(value, amountValue);
              } else {
                this.resetTransactionFee();
              }
              return [
                isValidAddress,
                this.context.intl.formatMessage(
                  apiErrorMessages.invalidAddress
                ),
              ];
            },
          ],
        },
        asset1: {
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
        walletsDropdown1: {
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

  get hasReceiverValue() {
    const receiverField = this.form.$('receiver1');
    return receiverField.value.length > 0;
  }

  get isReceiverValid() {
    const receiverField = this.form.$('receiver1');
    return receiverField.isValid;
  }

  get hasAssetValue() {
    const assetField = this.form.$('asset1');
    return !!assetField.value;
  }

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

  renderAssetRow = () => {};

  renderReceiverRow = (receiverId: string, index: number): Node => {
    const { intl } = this.context;
    const {
      isClearTooltipOpeningDownward,
      currencyMaxFractionalDigits,
      assets,
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
      asset,
      walletsDropdown,
      selectedNativeToken,
    } = sendFormFields[receiverId];

    const walletsDropdownFieldProps = walletsDropdown.bind();

    const receiverField = receiver;

    const assetFieldProps = asset.bind();

    const estimatedField = this.form.$('estimatedFee');

    const amount = new BigNumber(assetFieldProps.value || 0);

    const showReceiverLabelNumber = Object.keys(sendFormFields).length > 1;

    const receiverLabel = showReceiverLabelNumber
      ? `${intl.formatMessage(messages.receiverLabel)} #${index + 1}`
      : intl.formatMessage(messages.receiverLabel);

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

    const assetsSeparatorBasicHeight = 125;
    /* const assetsSeparatorCalculatedHeight = assets && assets.length ?
      (assetsSeparatorBasicHeight * assets.length) - 18 :
      assetsSeparatorBasicHeight; */

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
            ref={(input) => {
              this.receiverFieldRef[index] = input;
            }}
            error={receiverField.error}
            onChange={(value) => {
              receiverField.onChange(value || '');
              this.setState({
                isResetButtonDisabled: false,
              });
              if (value) {
                this.showReceiverField(index);
                this.renderAssetRow();
              }
            }}
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {this.hasReceiverValue && (
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
        {showReceiverField && showReceiverField[index] && (
          <>
            <div
              className={styles.fieldsLine}
              style={{
                height: `${assetsSeparatorBasicHeight}px`,
                top: `${assetsSeparatorBasicHeight - 10}px`,
                marginTop: `-${assetsSeparatorBasicHeight}px`,
              }}
            />
            <div className={styles.assetInput}>
              {selectedNativeToken &&
                selectedNativeToken.total &&
                selectedNativeToken.metadata && (
                  <div className={styles.amountTokenTotal}>
                    {intl.formatMessage(messages.ofLabel)}&nbsp;
                    {formattedWalletAmount(
                      new BigNumber(selectedNativeToken.total.quantity),
                      false
                    )}
                    &nbsp;{selectedNativeToken.metadata.acronym}
                  </div>
                )}
              <Fragment>
                <NumericInput
                  {...assetFieldProps}
                  className="asset"
                  ref={(input) => {
                    this.assetFieldRef[index] = input;
                  }}
                  label={intl.formatMessage(messages.assetLabel)}
                  bigNumberFormat={this.getCurrentNumberFormat()}
                  decimalPlaces={currencyMaxFractionalDigits}
                  numberLocaleOptions={{
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  }}
                  onChange={(value) => {
                    this._isCalculatingTransactionFee = true;
                    this.setState({
                      isResetButtonDisabled: false,
                    });
                    asset.onChange(value);
                    estimatedField.onChange(fees);
                    if (value) {
                      this.renderAssetRow();
                    }
                  }}
                  currency={
                    selectedNativeToken && selectedNativeToken.metadata
                      ? selectedNativeToken.metadata.acronym
                      : null
                  }
                  value={amount}
                  error={asset.error}
                  skin={AmountInputSkin}
                  onKeyPress={this.handleSubmitOnEnter}
                  allowSigns={false}
                />
                {this.hasAssetValue && (
                  <div className={styles.clearAssetContainer}>
                    <PopOver
                      content="Clear"
                      placement={
                        isClearTooltipOpeningDownward ? 'bottom' : 'top'
                      }
                    >
                      <button
                        onClick={() => this.clearAssetValue(index + 1)}
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
                    {...walletsDropdownFieldProps}
                    numberOfStakePools={4}
                    assets={assets}
                    onChange={(id) => this.onSelectAsset(id, index, receiverId)}
                    syncingLabel={intl.formatMessage(messages.syncingWallet)}
                    hasAssetsEnabled
                    value={selectedAssetId}
                    getStakePoolById={() => {}}
                    errorPosition="bottom"
                  />
                </div>
              </Fragment>
              <Button
                className={addAssetButtonClasses}
                label={intl.formatMessage(messages.addAssetButtonLabel)}
                onClick={() => this.addNewAssetRow(index, receiverId)}
                skin={ButtonSkin}
              />
            </div>
          </>
        )}
      </div>
    ) : null;
  };

  removeReceiverRow = (index: number, receiverId: string) => {
    this.clearAssetValue(index);
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
    const walletsDropdownFieldToDelete = `walletsDropdown${index}`;
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
      async ({ field, form }) => {
        const { value } = field;
        if (value === '') {
          this.resetTransactionFee();
          return [
            false,
            this.context.intl.formatMessage(messages.fieldIsRequired),
          ];
        }
        const receiverField = form.$(receiverId);
        const isAmountValid = receiverField.isValid;
        const isValidAddress = await this.props.addressValidator(value);
        if (isValidAddress && isAmountValid) {
          const amountValue = receiverField.value.toString();
          this.calculateTransactionFee(value, amountValue);
        } else {
          this.resetTransactionFee();
        }
        return [
          isValidAddress,
          this.context.intl.formatMessage(apiErrorMessages.invalidAddress),
        ];
      },
    ]);
  };

  addNewAssetField = (index: number, receiverId?: string) => {
    const newAsset = `asset${index}`;
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
        const { sendFormFields } = this.state;
        if (sendFormFields && sendFormFields[`receiver${index}`]) {
          const receiverField = form.$(`receiver${index}`);
          const receiverValue = receiverField.value;
          const isReceiverValid = receiverField.isValid;
          if (isValid && isReceiverValid) {
            this.calculateTransactionFee(receiverValue, amountValue);
          } else {
            this.resetTransactionFee();
          }
        }
        return [
          isValid,
          this.context.intl.formatMessage(messages.invalidAmount),
        ];
      },
    ]);
  };

  addNewWalletsDropdownField = (index: number, receiverId?: string) => {
    const newWalletsDropdown = `walletsDropdown${index}`;
    this.form.add({
      name: newWalletsDropdown,
      value: null,
      key: newWalletsDropdown,
    });
    this.form.$(newWalletsDropdown).set('type', 'select');
  };

  addNewReceiverRow = (index: number, receiverId: string) => {
    this.addNewReceiverField(receiverId);
    this.addNewAssetField(index);
    this.addNewWalletsDropdownField(index);
    this.showReceiverField(index - 1);
    this.setFormFields(false, index, receiverId);
  };

  addNewAssetRow = (index: number, receiverId: string) => {
    this.addNewAssetField(index, receiverId);
    this.addNewWalletsDropdownField(index, receiverId);
    this.setFormFields(false, index, receiverId);
  };

  onSelectAsset = (assetId: string, id: number, receiverId: string) => {
    this.setState({ selectedAssetId: assetId });
    this.updateFormFields(id, assetId, receiverId);
  };

  getNativeTokenById = (selectedAssetId: string): ?Asset => {
    const { assets } = this.props;
    return assets.find((asset) => asset.id === selectedAssetId);
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
      showReceiverField,
      isResetButtonDisabled,
      sendFormFields,
    } = this.state;
    const assetField = form.$('asset1');
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

    const newReceiverButtonClasses = classNames([
      styles.addNewReceiverButton,
      'flat',
    ]);

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
              {showReceiverField &&
                showReceiverField[0] &&
                showReceiverField.length > 0 && (
                  <Button
                    className={newReceiverButtonClasses}
                    label={intl.formatMessage(
                      messages.addNewReceiverButtonLabel
                    )}
                    onClick={() =>
                      this.addNewReceiverRow(
                        Object.keys(sendFormFields).length + 1,
                        `receiver${Object.keys(sendFormFields).length + 1}`
                      )
                    }
                    skin={ButtonSkin}
                  />
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

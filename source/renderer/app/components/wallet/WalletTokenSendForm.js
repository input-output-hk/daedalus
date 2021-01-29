// @flow
import React, { Component } from 'react';
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
import styles from './WalletTokenSendForm.scss';
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
import WalletTokenSendConfirmationDialog from './WalletTokenSendConfirmationDialog';
import Wallet from '../../domains/Wallet';
import closeIcon from '../../assets/images/close-cross.inline.svg';
import WalletsDropdown from '../widgets/forms/WalletsDropdown';

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
    defaultMessage: '!!!Asset',
    description: 'Label for the "Asset" number input in the wallet send form.',
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
    defaultMessage: '!!!+ Add another asset',
    description:
      'Label for the "+ Add another asset" button in the wallet send form.',
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
  walletAmount: BigNumber,
  addressValidator: Function,
  openDialogAction: Function,
  isDialogOpen: Function,
  onExternalLinkClick?: Function,
  isRestoreActive: boolean,
  hwDeviceStatus: HwDeviceStatus,
  isHardwareWallet: boolean,
  nativeTokens: Array<Wallet>,
  isClearTooltipOpeningDownward?: boolean,
  selectedWallet: ?Wallet,
};

type State = {
  isTransactionFeeCalculated: boolean,
  transactionFee: BigNumber,
  feeCalculationRequestQue: number,
  transactionFeeError: ?string | ?Node,
  showReceiverRemoveBtn: boolean,
  selectedWalletId: ?string,
  showReceiverField: boolean,
  tokens: Array<Wallet>,
};

@observer
export default class WalletTokenSendForm extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    isTransactionFeeCalculated: false,
    transactionFee: new BigNumber(0),
    feeCalculationRequestQue: 0,
    transactionFeeError: null,
    showReceiverRemoveBtn: false,
    selectedWalletId: null,
    showReceiverField: false,
    tokens: [],
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
    const { selectedWallet, nativeTokens } = this.props;
    if (selectedWallet) {
      this.onSelectWallet(selectedWallet.id);
    }

    // @TODO Remove hardcoded setter of values for token currencies
    if (nativeTokens && nativeTokens.length) {
      this.setState({
        tokens: [...nativeTokens],
      });
    }
  }

  componentWillUnmount() {
    this._isMounted = false;
  }

  handleOnSubmit = () => {
    if (this.isDisabled()) {
      return false;
    }
    this.props.openDialogAction({
      dialog: WalletTokenSendConfirmationDialog,
    });
  };

  clearReceiverAddress = () => {
    const receiverField = this.form.$('receiver');
    receiverField.clear();
    receiverField.focus();
  };

  clearAssetValue = () => {
    const assetField = this.form.$('asset');
    assetField.clear();
    assetField.focus();
  };

  handleOnReset = () => {
    this.form.reset();
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.handleOnSubmit);

  isDisabled = () =>
    this._isCalculatingTransactionFee || !this.state.isTransactionFeeCalculated;

  // FORM VALIDATION
  form = new ReactToolboxMobxForm(
    {
      fields: {
        receiver: {
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
              const assetField = form.$('asset');
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
        asset: {
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
        walletsDropdown: {
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

  showReceiverField = () => {
    this.setState({
      showReceiverField: true,
    });
  };

  hideReceiverField = () => {
    this.setState({
      showReceiverField: false,
    });
  };

  get hasReceiverValue() {
    const receiverField = this.form.$('receiver');
    return receiverField.value.length > 0;
  }

  get isReceiverValid() {
    const receiverField = this.form.$('receiver');
    return receiverField.isValid;
  }

  get hasAssetValue() {
    const assetField = this.form.$('asset');
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

  renderReceiverRow = () => {};

  removeReceiverRow = () => {
    this.hideReceiverField();
    this.clearAssetValue();
    this.clearReceiverAddress();
  };

  addAssetRow = () => {};

  onSelectWallet = (walletId: string) => {
    this.setState({ selectedWalletId: walletId });
  };

  getNativeTokenWalletById = (selectedWalletId: string): ?Wallet => {
    const { tokens } = this.state;
    return tokens.find((token) => token.id === selectedWalletId);
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
      isClearTooltipOpeningDownward,
    } = this.props;

    const {
      isTransactionFeeCalculated,
      transactionFee,
      transactionFeeError,
      showReceiverRemoveBtn,
      selectedWalletId,
      showReceiverField,
      tokens,
    } = this.state;
    const assetField = form.$('asset');
    const receiverField = form.$('receiver');
    const estimatedField = form.$('estimatedFee');
    const walletsDropdownField = form.$('walletsDropdown');
    const receiverFieldProps = receiverField.bind();
    const assetFieldProps = assetField.bind();
    const estimatedFieldProps = estimatedField.bind();
    const walletsDropdownFieldProps = walletsDropdownField.bind();

    const amount = new BigNumber(assetFieldProps.value || 0);

    let fees = null;
    let total = null;
    if (isTransactionFeeCalculated) {
      fees = transactionFee.toFormat(currencyMaxFractionalDigits);
      total = amount.add(transactionFee).toFormat(currencyMaxFractionalDigits);
    }

    const selectedNativeToken: any =
      selectedWalletId && tokens && tokens.length
        ? this.getNativeTokenWalletById(selectedWalletId)
        : null;

    const removeReceiverButtonClasses = classNames([
      styles.removeReceiverButton,
      'flat',
      showReceiverRemoveBtn ? styles.active : null,
    ]);

    const newReceiverButtonClasses = classNames([
      styles.addNewReceiverButton,
      'flat',
    ]);

    const addAssetButtonClasses = classNames([
      styles.addAssetButton,
      'primary',
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
            <div className={styles.walletTokenSendForm}>
              <div className={styles.walletTokenSendFormContainer}>
                <div className={styles.fieldsContainer}>
                  <div
                    onMouseEnter={this.showRemoveButton}
                    onMouseLeave={this.hideRemoveButton}
                    className={styles.receiverInput}
                  >
                    {showReceiverField && (
                      <Button
                        className={removeReceiverButtonClasses}
                        label={intl.formatMessage(
                          messages.removeReceiverButtonLabel
                        )}
                        onClick={this.removeReceiverRow}
                        skin={ButtonSkin}
                      />
                    )}
                    <Input
                      className="receiver"
                      label={intl.formatMessage(messages.receiverLabel)}
                      {...receiverField.bind()}
                      error={receiverField.error}
                      onChange={(value) => {
                        receiverField.onChange(value || '');
                        if (value) {
                          this.showReceiverField();
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
                          placement={
                            isClearTooltipOpeningDownward ? 'bottom' : 'top'
                          }
                        >
                          <button
                            onClick={() => this.clearReceiverAddress()}
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
                  {this.hasReceiverValue && showReceiverField && (
                    <>
                      <div className={styles.fieldsLine} />
                      <div className={styles.assetInput}>
                        {selectedNativeToken && (
                          <div className={styles.amountTokenTotal}>
                            {intl.formatMessage(messages.ofLabel)}&nbsp;
                            {formattedWalletAmount(
                              selectedNativeToken.amount,
                              false
                            )}
                            &nbsp;{selectedNativeToken.ticker}
                          </div>
                        )}
                        <NumericInput
                          {...assetFieldProps}
                          className="asset"
                          label={intl.formatMessage(messages.assetLabel)}
                          numberFormat={this.getCurrentNumberFormat()}
                          numberLocaleOptions={{
                            minimumFractionDigits: currencyMaxFractionalDigits,
                          }}
                          onChange={(value) => {
                            this._isCalculatingTransactionFee = true;
                            assetField.onChange(value);
                            estimatedField.onChange(fees);
                            if (value) {
                              this.renderAssetRow();
                            }
                          }}
                          currency={selectedNativeToken.ticker}
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
                                onClick={() => this.clearAssetValue()}
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
                            wallets={tokens}
                            onChange={(id) => this.onSelectWallet(id)}
                            syncingLabel={intl.formatMessage(
                              messages.syncingWallet
                            )}
                            hasNativeTokens
                            value={selectedWalletId}
                            getStakePoolById={() => {}}
                            errorPosition="bottom"
                          />
                        </div>
                        <Button
                          className={addAssetButtonClasses}
                          label={intl.formatMessage(
                            messages.addAssetButtonLabel
                          )}
                          onClick={this.addAssetRow}
                          skin={ButtonSkin}
                        />
                      </div>
                    </>
                  )}
                </div>
              </div>
              {this.isReceiverValid && showReceiverField && (
                <Button
                  className={newReceiverButtonClasses}
                  label={intl.formatMessage(messages.addNewReceiverButtonLabel)}
                  onClick={this.renderReceiverRow}
                  skin={ButtonSkin}
                />
              )}
              <div className={styles.estimatedFeeInput}>
                <NumericInput
                  {...estimatedFieldProps}
                  className={styles.estimatedFee}
                  label={intl.formatMessage(messages.estimatedFeeLabel)}
                  numberFormat={this.getCurrentNumberFormat()}
                  numberLocaleOptions={{
                    minimumFractionDigits: currencyMaxFractionalDigits,
                  }}
                  error={transactionFeeError || assetField.error}
                  currency={intl.formatMessage(globalMessages.unitAda)}
                  fees={fees}
                  total={total}
                  skin={AmountInputSkin}
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
                  onClick={this.handleOnReset}
                  skin={ButtonSkin}
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

        {isDialogOpen(WalletTokenSendConfirmationDialog) ? (
          <WalletSendConfirmationDialogContainer
            amount={amount.toFormat(currencyMaxFractionalDigits)}
            receiver={receiverFieldProps.value}
            receivers={[receiverFieldProps.value, receiverFieldProps.value]}
            totalAmount={total}
            transactionFee={fees}
            amountToNaturalUnits={formattedAmountToNaturalUnits}
            currencyUnit={selectedNativeToken.ticker}
            onExternalLinkClick={onExternalLinkClick}
            hwDeviceStatus={hwDeviceStatus}
            isHardwareWallet={isHardwareWallet}
            nativeTokens={tokens}
          />
        ) : null}
      </div>
    );
  }
}

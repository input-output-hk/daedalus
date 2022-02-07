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
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSendAssetsConfirmation... Remove this comment to see the full error message
import styles from './WalletSendAssetsConfirmationDialog.scss';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
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
import { hasTokensLeftAfterTransaction } from '../../../utils/assets';
import globalMessages from '../../../i18n/global-messages';

const SHOW_TOTAL_AMOUNT = false;
type Props = {
  amount: string;
  receiver: string;
  wallet: Wallet;
  totalAmount: BigNumber;
  selectedAssets: Array<AssetToken>;
  allAvailableTokens: Array<AssetToken>;
  assetsAmounts: Array<string>;
  transactionFee: string | null | undefined;
  onSubmit: (...args: Array<any>) => any;
  amountToNaturalUnits: (amountWithFractions: string) => string;
  onCancel: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
  isSubmitting: boolean;
  isFlight: boolean;
  error: LocalizableError | null | undefined;
  hwDeviceStatus: HwDeviceStatus;
  isHardwareWallet: boolean;
  onInitiateTransaction: (...args: Array<any>) => any;
  onCopyAssetParam: (...args: Array<any>) => any;
  isTrezor: boolean;
  formattedTotalAmount: string;
};
type State = {
  selectedAssets: Array<AssetToken>;
  assetsAmounts: Array<string>;
  areTermsAccepted: boolean;
};
const messages = getMessages();

@observer
class WalletSendAssetsConfirmationDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    selectedAssets: [],
    assetsAmounts: [],
    areTermsAccepted: false,
  };

  componentDidMount() {
    // We need to keep initial list of selectedAssets and their amounts as a state
    // value to avoid losing them after the transaction is confirmed
    // (this affects only hardware wallets for which we close the dialog
    // after transaction has been confirmed)
    const { selectedAssets, assetsAmounts } = this.props;
    this.setState({
      selectedAssets,
      assetsAmounts,
    });
  }

  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
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
      plugins: {
        vjf: vjf(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );
  submit = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'submit' does not exist on type 'ReactToo... Remove this comment to see the full error message
    this.form.submit({
      onSuccess: (form) => {
        const { selectedAssets, assetsAmounts } = this.state;
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
          assets: selectedAssets,
          assetsAmounts,
        };
        this.props.onSubmit(transactionData);
      },
      onError: () => {},
    });
  };
  handleSubmitOnEnter = (event: KeyboardEvent) =>
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    (this.props.isHardwareWallet || this.form.$('passphrase').isValid) &&
    submitOnEnter(this.submit, event);
  renderConfirmationElement = (
    isHardwareWallet: boolean
  ): React.ReactElement<React.ComponentProps<any>, any> | null => {
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const passphraseField = this.form.$('passphrase');
    const { areTermsAccepted } = this.state;
    const {
      hwDeviceStatus,
      isFlight,
      onExternalLinkClick,
      wallet,
      isTrezor,
    } = this.props;
    let returnJSX = null;

    if (!isFlight || (isFlight && areTermsAccepted)) {
      const { name } = wallet;
      returnJSX = isHardwareWallet ? (
        <div className={styles.hardwareWalletStatusWrapper}>
          <HardwareWalletStatus
            hwDeviceStatus={hwDeviceStatus}
            walletName={name}
            isTrezor={isTrezor}
            onExternalLinkClick={onExternalLinkClick}
          />
        </div>
      ) : (
        <Input
          type="password"
          className={styles.passphrase}
          {...passphraseField.bind()}
          error={passphraseField.error}
          skin={InputSkin}
          onKeyPress={this.handleSubmitOnEnter}
          autoFocus
        />
      );
    }

    return returnJSX;
  };
  onCheckboxClick = (areTermsAccepted: boolean) => {
    const { isHardwareWallet, onInitiateTransaction } = this.props;
    this.setState({
      areTermsAccepted,
    });

    if (isHardwareWallet) {
      onInitiateTransaction();
    }
  };
  getAssetAmount = (index: number) => {
    const { assetsAmounts } = this.state;
    return get(assetsAmounts, index, 0);
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
    const { selectedAssets, areTermsAccepted, assetsAmounts } = this.state;
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const passphraseField = form.$('passphrase');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
    const {
      onCancel,
      allAvailableTokens,
      amount,
      receiver,
      transactionFee,
      isSubmitting,
      isFlight,
      error,
      onExternalLinkClick,
      hwDeviceStatus,
      isHardwareWallet,
      onCopyAssetParam,
      wallet,
      formattedTotalAmount,
      totalAmount,
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
    const assetsSeparatorCalculatedHeight = selectedAssets.length
      ? assetsSeparatorBasicHeight * selectedAssets.length * 2 - 18
      : assetsSeparatorBasicHeight;
    let errorElement = null;

    if (error) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'values' does not exist on type 'Localiza... Remove this comment to see the full error message
      const errorHasLink = !!error.values.linkLabel;
      errorElement = errorHasLink ? (
        <FormattedHTMLMessageWithLink
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
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
        {shouldShowEmptyWalletWarning(
          totalAmount,
          wallet,
          !!allAvailableTokens?.length &&
            allAvailableTokens.length > 0 &&
            hasTokensLeftAfterTransaction(
              allAvailableTokens,
              selectedAssets,
              assetsAmounts
            )
        ) && (
          <div className={styles.warning}>
            <FormattedHTMLMessage {...messages.emptyingWarning} tagName="p" />
          </div>
        )}

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
                      <span>{intl.formatMessage(globalMessages.adaName)}</span>
                    </h3>
                    <div className={styles.amountFeesWrapper}>
                      <div className={styles.amount}>
                        {amount} {intl.formatMessage(globalMessages.adaUnit)}
                      </div>
                    </div>
                  </div>
                  {selectedAssets.map((asset, assetIndex) => {
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
                              onCopyAssetParam={onCopyAssetParam}
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
                                    {...messages[
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
                  <span>
                    &nbsp;{intl.formatMessage(globalMessages.adaUnit)}
                  </span>
                </div>
              </div>

              <div className={styles.feesWrapper}>
                <div className={styles.feesLabel}>
                  {intl.formatMessage(messages.feesLabel)}
                </div>
                <div className={styles.fees}>
                  +{transactionFee}
                  <span>
                    &nbsp;{intl.formatMessage(globalMessages.adaUnit)}
                  </span>
                </div>
              </div>
            </div>

            <div className={styles.totalAmountLabel}>
              {intl.formatMessage(messages.totalLabel)}
            </div>
            <div className={styles.totalAmount}>
              {formattedTotalAmount}
              <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
            </div>
          </>
        ) : (
          <div className={styles.feesWrapper}>
            <div className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </div>
            <div className={styles.fees}>
              +{transactionFee}
              <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
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
        {errorElement ? <p className={styles.error}>{errorElement}</p> : null}
      </Dialog>
    );
  }
}

export default WalletSendAssetsConfirmationDialog;

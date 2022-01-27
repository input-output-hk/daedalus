import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { FormattedHTMLMessage, intlShape } from 'react-intl';
import vjf from 'mobx-react-form/lib/validators/VJF';
import BigNumber from 'bignumber.js';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSendConfirmationDialog... Remove this comment to see the full error message
import styles from './WalletSendConfirmationDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { submitOnEnter } from '../../../utils/form';
import { FormattedHTMLMessageWithLink } from '../../widgets/FormattedHTMLMessageWithLink';
import HardwareWalletStatus from '../../hardware-wallet/HardwareWalletStatus';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import type { HwDeviceStatus } from '../../../domains/Wallet';
import Wallet, { HwDeviceStatuses } from '../../../domains/Wallet';
import { getMessages } from './WalletSendAssetsConfirmationDialog.messages';
import { shouldShowEmptyWalletWarning } from '../../../utils/walletUtils';
import type { AssetToken } from '../../../api/assets/types';
import globalMessages from '../../../i18n/global-messages';

type Props = {
  amount: string;
  receiver: string;
  wallet: Wallet;
  totalAmount: BigNumber;
  allAvailableTokens?: Array<AssetToken>;
  transactionFee: string | null | undefined;
  onSubmit: (...args: Array<any>) => any;
  amountToNaturalUnits: (amountWithFractions: string) => string;
  onCancel: (...args: Array<any>) => any;
  isSubmitting: boolean;
  isFlight: boolean;
  error: LocalizableError | null | undefined;
  hwDeviceStatus: HwDeviceStatus;
  isHardwareWallet: boolean;
  onInitiateTransaction: (...args: Array<any>) => any;
  onExternalLinkClick: (...args: Array<any>) => any;
  isTrezor: boolean;
  formattedTotalAmount: string;
};
type State = {
  areTermsAccepted: boolean;
};
const messages = getMessages();

@observer
class WalletSendConfirmationDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    areTermsAccepted: false,
  };
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
  ): React.ReactElement<React.ComponentProps<any>, any> | null | undefined => {
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

    if (!isFlight || (isFlight && areTermsAccepted)) {
      const { name } = wallet;
      return isHardwareWallet ? (
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

    return null;
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

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { areTermsAccepted } = this.state;
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
          !!allAvailableTokens?.length && allAvailableTokens.length > 0
        ) && (
          <div className={styles.warning}>
            <FormattedHTMLMessage {...messages.emptyingWarning} tagName="p" />
          </div>
        )}
        <div className={styles.addressToLabelWrapper}>
          <div className={styles.addressToLabel}>
            {intl.formatMessage(messages.addressToLabel)}
          </div>
          <div className={styles.addressTo}>{receiver}</div>
        </div>

        <div className={styles.amountFeesWrapper}>
          <div className={styles.amountWrapper}>
            <div className={styles.amountLabel}>
              {intl.formatMessage(messages.amountLabel)}
            </div>
            <div className={styles.amount}>
              {amount}
              <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
            </div>
          </div>

          <div className={styles.feesWrapper}>
            <div className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </div>
            <div className={styles.fees}>
              +{transactionFee}
              <span>&nbsp;{intl.formatMessage(globalMessages.adaUnit)}</span>
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

export default WalletSendConfirmationDialog;

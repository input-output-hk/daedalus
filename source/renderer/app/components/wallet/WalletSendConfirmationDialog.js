// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import ReactToolboxMobxForm from '../../utils/ReactToolboxMobxForm';
import Dialog from '../widgets/Dialog';
import DialogCloseButton from '../widgets/DialogCloseButton';
import globalMessages from '../../i18n/global-messages';
import LocalizableError from '../../i18n/LocalizableError';
import styles from './WalletSendConfirmationDialog.scss';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../config/timingConfig';
import { submitOnEnter } from '../../utils/form';
import { FormattedHTMLMessageWithLink } from '../widgets/FormattedHTMLMessageWithLink';

export const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.send.confirmationDialog.title',
    defaultMessage: '!!!Confirm transaction',
    description: 'Title for the "Confirm transaction" dialog.',
  },
  passphraseLabel: {
    id: 'wallet.send.confirmationDialog.passphraseLabel',
    defaultMessage: '!!!Spending password',
    description:
      'Label for the "Spending password" input in the wallet send confirmation dialog.',
  },
  addressToLabel: {
    id: 'wallet.send.confirmationDialog.addressToLabel',
    defaultMessage: '!!!To',
    description: 'Label for the "To" in the wallet send confirmation dialog.',
  },
  amountLabel: {
    id: 'wallet.send.confirmationDialog.amountLabel',
    defaultMessage: '!!!Amount',
    description:
      'Label for the "Amount" in the wallet send confirmation dialog.',
  },
  feesLabel: {
    id: 'wallet.send.confirmationDialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Label for the "Fees" in the wallet send confirmation dialog.',
  },
  totalLabel: {
    id: 'wallet.send.confirmationDialog.totalLabel',
    defaultMessage: '!!!Total',
    description:
      'Label for the "Total" in the wallet send confirmation dialog.',
  },
  passphraseFieldPlaceholder: {
    id: 'wallet.send.confirmationDialog.passphraseFieldPlaceholder',
    defaultMessage: '!!!Type your spending password',
    description:
      'Placeholder for the "Spending password" inputs in the wallet send confirmation dialog.',
  },
  flightCandidateWarning: {
    id: 'wallet.send.confirmationDialog.flightCandidateWarning',
    defaultMessage:
      '!!!{Warning}, flight candidate versions of Daedalus are connected to Cardano mainnet. If you confirm this transaction, your ada will be sent for real.',
    description:
      'Text for the "Flight candidate" warning in the wallet send confirmation dialog.',
  },
  flightCandidateCheckboxLabel: {
    id: 'wallet.send.confirmationDialog.flightCandidateCheckboxLabel',
    defaultMessage:
      '!!!I understand that real ada will be moved as part of this transaction and that this action is irreversible.',
    description:
      'Label for the "Flight candidate" warning checkbox in the wallet send confirmation dialog.',
  },
  sendButtonLabel: {
    id: 'wallet.send.confirmationDialog.submit',
    defaultMessage: '!!!Send',
    description:
      'Label for the send button in the wallet send confirmation dialog.',
  },
  backButtonLabel: {
    id: 'wallet.send.confirmationDialog.back',
    defaultMessage: '!!!Back',
    description:
      'Label for the back button in the wallet send confirmation dialog.',
  },
  passwordErrorMessage: {
    id: 'wallet.send.confirmationDialog.passwordError',
    defaultMessage: '!!!Incorrect spending password.',
    description:
      'Label for password error in the wallet send confirmation dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  amount: string,
  receiver: string,
  totalAmount: ?string,
  transactionFee: ?string,
  onSubmit: Function,
  amountToNaturalUnits: (amountWithFractions: string) => string,
  onCancel: Function,
  onExternalLinkClick: Function,
  isSubmitting: boolean,
  error: ?LocalizableError,
  currencyUnit: string,
};

@observer
export default class WalletSendConfirmationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
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
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { receiver, amount, amountToNaturalUnits } = this.props;
        const { passphrase } = form.values();
        const transactionData = {
          receiver,
          amount: amountToNaturalUnits(amount),
          passphrase,
        };
        this.props.onSubmit(transactionData);
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = (event: {}) =>
    this.form.$('passphrase').isValid && submitOnEnter(this.submit, event);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const passphraseField = form.$('passphrase');
    const flightCandidateCheckboxField = form.$('flightCandidateCheckbox');
    const {
      onCancel,
      amount,
      receiver,
      totalAmount,
      transactionFee,
      isSubmitting,
      error,
      currencyUnit,
      onExternalLinkClick,
    } = this.props;

    const confirmButtonClasses = classnames([
      'confirmButton',
      isSubmitting ? styles.submitButtonSpinning : null,
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.backButtonLabel),
        onClick: !isSubmitting ? onCancel : () => {},
      },
      {
        label: intl.formatMessage(messages.sendButtonLabel),
        onClick: this.submit,
        primary: true,
        className: confirmButtonClasses,
        disabled:
          !passphraseField.isValid || !flightCandidateCheckboxField.value,
      },
    ];

    let errorElement = null;
    if (error) {
      const errorHasLink = !!error.values.linkLabel;
      errorElement = errorHasLink ? (
        <FormattedHTMLMessageWithLink
          message={error}
          onExternalLinkClick={onExternalLinkClick}
        />
      ) : (
        intl.formatMessage(error)
      );
    }

    return (
      <Dialog
        title={intl.formatMessage(messages.dialogTitle)}
        actions={actions}
        closeOnOverlayClick
        primaryButtonAutoFocus
        onClose={!isSubmitting ? onCancel : () => {}}
        className={styles.dialog}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.passphraseFields}>
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
                <span className={styles.currencySymbol}>
                  &nbsp;{currencyUnit}
                </span>
              </div>
            </div>

            <div className={styles.feesWrapper}>
              <div className={styles.feesLabel}>
                {intl.formatMessage(messages.feesLabel)}
              </div>
              <div className={styles.fees}>
                +{transactionFee}
                <span className={styles.currencySymbol}>
                  &nbsp;{currencyUnit}
                </span>
              </div>
            </div>
          </div>

          <div className={styles.totalAmountWrapper}>
            <div className={styles.totalAmountLabel}>
              {intl.formatMessage(messages.totalLabel)}
            </div>
            <div className={styles.totalAmount}>
              {totalAmount}
              <span className={styles.currencySymbol}>
                &nbsp;{currencyUnit}
              </span>
            </div>
          </div>

          <Input
            type="password"
            className={styles.passphrase}
            {...passphraseField.bind()}
            error={passphraseField.error}
            skin={InputSkin}
            onKeyPress={this.handleSubmitOnEnter}
            autoFocus
          />
        </div>

        <div className={styles.flightCandidateWarning}>
          <FormattedHTMLMessage
            {...messages.flightCandidateWarning}
            tagName="p"
          />
          {/* <p>{intl.formatMessage(messages.flightCandidateWarning)}</p> */}
          <Checkbox
            {...flightCandidateCheckboxField.bind()}
            error={flightCandidateCheckboxField.error}
            skin={CheckboxSkin}
          />
        </div>

        {errorElement ? <p className={styles.error}>{errorElement}</p> : null}
      </Dialog>
    );
  }
}

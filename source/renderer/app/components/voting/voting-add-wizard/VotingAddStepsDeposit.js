// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import commonStyles from './VotingAddSteps.scss';
import styles from './VotingAddStepsDeposit.scss';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { formattedWalletAmount } from '../../../utils/formatters';
import globalMessages from '../../../i18n/global-messages';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import BigNumber from 'bignumber.js';
import { submitOnEnter } from '../../../utils/form';
import { observer } from 'mobx-react';
import LocalizableError from '../../../i18n/LocalizableError';

const messages = defineMessages({
  description: {
    id: 'voting.votingAdd.Deposit.step.description',
    defaultMessage:
      '!!!By confirming this action, you are making a tx to yourself, and generating meta-data that validates your voting power.',
    description: 'Description on the voting add "deposit" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingAdd.Deposit.step.continueButtonLabel',
    defaultMessage: '!!!Deposit funds and generate QR code',
    description: 'Label for continue button on the voting add "deposit" step.',
  },
  feesLabel: {
    id: 'voting.votingAdd.Deposit.step.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label on the voting add "deposit" step.',
  },
  spendingPasswordPlaceholder: {
    id: 'voting.votingAdd.Deposit.step.spendingPasswordPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'voting.votingAdd.Deposit.step.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  calculatingFees: {
    id: 'voting.votingAdd.Deposit.step.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description: '"Calculating fees" message in the "deposit" step.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onConfirm: Function,
  transactionFee: ?BigNumber,
  error: ?LocalizableError,
};

@observer
export default class VotingAddStepsDeposit extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        spendingPassword: {
          type: 'password',
          label: this.context.intl.formatMessage(
            messages.spendingPasswordLabel
          ),
          placeholder: this.context.intl.formatMessage(
            messages.spendingPasswordPlaceholder
          ),
          value: '',
          validators: [
            ({ field }) => {
              const password = field.value;
              if (password === '') {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: (form) => {
        const { spendingPassword } = form.values();
        this.props.onConfirm(spendingPassword);
      },
      onError: (error) => {},
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { transactionFee, error } = this.props;
    const spendingPasswordField = form.$('spendingPassword');
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);

    const className = classNames([
      commonStyles.votingAddSteps,
      styles.votingAddStepsDepositWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    return (
      <div className={className}>
        <div className={contentClassName}>
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
          </p>

          <div className={styles.feesWrapper}>
            <p className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </p>
            <p className={styles.feesAmount}>
              {!transactionFee ? (
                <span className={styles.calculatingFeesLabel}>
                  {intl.formatMessage(messages.calculatingFees)}
                </span>
              ) : (
                <>
                  <span>{formattedWalletAmount(transactionFee, false)}</span>
                  <span className={styles.feesAmountLabel}>
                    &nbsp;{intl.formatMessage(globalMessages.unitAda)}
                  </span>
                </>
              )}
            </p>
          </div>

          <Input
            {...spendingPasswordField.bind()}
            autoFocus={true}
            skin={InputSkin}
            error={spendingPasswordField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {error ? (
            <div className={styles.errorMessage}>
              <p>{intl.formatMessage(error)}</p>
            </div>
          ) : null}
        </div>

        <Button
          skin={ButtonSkin}
          label={buttonLabel}
          onClick={this.submit}
          disabled={!spendingPasswordField.isValid || !transactionFee}
        />
      </div>
    );
  }
}

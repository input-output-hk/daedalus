// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import vjf from 'mobx-react-form/lib/validators/VJF';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import commonStyles from './VotingAddSteps.scss';
import styles from './VotingAddStepsSign.scss';

const messages = defineMessages({
  description: {
    id: 'voting.votingAdd.sign.step.description',
    defaultMessage:
      "!!!By submitting this registration transaction. You will be creating a proof of your staking balance. The proof will be used to calculate your voting power. Except fees, there will no change in your wallet's balance.",
    description: 'Description on the voting add "sign" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingAdd.sign.step.continueButtonLabel',
    defaultMessage: '!!!Submit Registration Transaction',
    description: 'Label for continue button on the voting add "sign" step.',
  },
  feesLabel: {
    id: 'voting.votingAdd.sign.step.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label on the voting add "sign" step.',
  },
  spendingPasswordPlaceholder: {
    id: 'voting.votingAdd.sign.step.spendingPasswordPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'voting.votingAdd.sign.step.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  calculatingFees: {
    id: 'voting.votingAdd.sign.step.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description: '"Calculating fees" message in the "sign" step.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onConfirm: Function,
  transactionFee: ?BigNumber,
  transactionFeeError: string | Node | null,
};

@observer
export default class VotingAddStepsSign extends Component<Props> {
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
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { transactionFee, transactionFeeError } = this.props;
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
            autoFocus
            skin={InputSkin}
            error={spendingPasswordField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
          {transactionFeeError ? (
            <div className={styles.errorMessage}>
              <p>{transactionFeeError}</p>
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

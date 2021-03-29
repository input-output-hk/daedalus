// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import vjf from 'mobx-react-form/lib/validators/VJF';
import BigNumber from 'bignumber.js';
import { observer } from 'mobx-react';
import { Link } from 'react-polymorph/lib/components/Link';
import { LinkSkin } from 'react-polymorph/lib/skins/simple/LinkSkin';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { formattedWalletAmount } from '../../../utils/formatters';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './VotingRegistrationStepsRegister.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.register.step.description',
    defaultMessage:
      '!!!Please sign the voting registration transaction. This transaction links your wallet balance with your Fund3 voting registration, as a proof of your voting power. Funds will not leave your wallet, but registration requires paying transaction fees, as displayed on-screen.',
    description: 'Description on the voting registration "sign" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.register.step.continueButtonLabel',
    defaultMessage: '!!!Submit registration transaction',
    description:
      'Label for continue button on the voting registration "sign" step.',
  },
  feesLabel: {
    id: 'voting.votingRegistration.register.step.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label on the voting registration "sign" step.',
  },
  spendingPasswordPlaceholder: {
    id: 'voting.votingRegistration.register.step.spendingPasswordPlaceholder',
    defaultMessage: '!!!Spending password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'voting.votingRegistration.register.step.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  calculatingFees: {
    id: 'voting.votingRegistration.register.step.calculatingFees',
    defaultMessage: '!!!Calculating fees',
    description: '"Calculating fees" message in the "sign" step.',
  },
  learnMoreLink: {
    id: 'voting.votingRegistration.register.step.learnMoreLink',
    defaultMessage: '!!!Learn more',
    description: '"Learn more" link on the "sign" step.',
  },
  learntMoreLinkUrl: {
    id: 'voting.votingRegistration.register.step.learntMoreLinkUrl',
    defaultMessage: '!!!https://cardano.ideascale.com/a/index',
    description: 'Learn more" link URL on the "sign" step.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  stepsList: Array<string>,
  activeStep: number,
  transactionFee: ?BigNumber,
  transactionFeeError?: string | Node | null,
  transactionError?: ?LocalizableError,
  isSubmitting: boolean,
  onConfirm: Function,
  onClose: Function,
  onBack: Function,
  onExternalLinkClick: Function,
};

@observer
export default class VotingRegistrationStepsRegister extends Component<Props> {
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
    const {
      transactionFee,
      transactionFeeError,
      transactionError,
      isSubmitting,
      onExternalLinkClick,
      onClose,
      onBack,
      stepsList,
      activeStep,
    } = this.props;
    const spendingPasswordField = form.$('spendingPassword');
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const learnMoreLinkUrl = intl.formatMessage(messages.learntMoreLinkUrl);

    const actions = [
      {
        className: isSubmitting ? styles.isSubmitting : null,
        label: buttonLabel,
        onClick: this.submit,
        disabled:
          !spendingPasswordField.isValid || !transactionFee || isSubmitting,
        primary: true,
      },
    ];

    return (
      <VotingRegistrationDialog
        onClose={() => {
          onClose();
        }}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        onBack={onBack}
        containerClassName={styles.component}
      >
        <p className={styles.description}>
          <FormattedHTMLMessage {...messages.description} />
        </p>

        <div className={styles.learnMoreWrapper}>
          <Link
            className={styles.externalLink}
            onClick={(event) => onExternalLinkClick(learnMoreLinkUrl, event)}
            label={intl.formatMessage(messages.learnMoreLink)}
            skin={LinkSkin}
          />
        </div>

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

        {transactionError ? (
          <div className={styles.errorMessage}>
            <p>{intl.formatMessage(transactionError)}</p>
          </div>
        ) : null}
      </VotingRegistrationDialog>
    );
  }
}

// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  defineMessages,
  intlShape,
  FormattedMessage,
  FormattedHTMLMessage,
} from 'react-intl';
import classNames from 'classnames';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import commonStyles from './DelegationSteps.scss';
import styles from './DelegationStepsActivationDialog.scss';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import { submitOnEnter } from '../../../utils/form';
import globalMessages from '../../../i18n/global-messages';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.activation.step.dialog.title',
    defaultMessage: '!!!Confirm Delegation',
    description:
      'Title "Confirm Delegation" on the delegation setup "activation" step dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator labe on the delegation setup "activation" step dialog.',
  },
  descriptionLine1: {
    id: 'staking.delegationSetup.activation.step.dialog.description.line1',
    defaultMessage:
      '!!!All <span>new</span> wallet addresses will now match your delegation preferences and the stake associated with ada received on those addresses will be delegated. ',
    description:
      'Description "line 1" on the delegation setup "activation" step dialog.',
  },
  descriptionLine2: {
    id: 'staking.delegationSetup.activation.step.dialog.description.line2',
    defaultMessage:
      '!!!To delegate ada which remains in old addresses in your wallet, move it to a new address where your delegation preferences have been applied.',
    description:
      'Description "line 2" on the delegation setup "activation" step dialog.',
  },
  addressLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.addressLabel',
    defaultMessage: '!!!To',
    description:
      'Address label on the delegation setup "activation" step dialog.',
  },
  amountLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.amountLabel',
    defaultMessage: '!!!Amount',
    description:
      'Amount label on the delegation setup "activation" step dialog.',
  },
  feesLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description: 'Fees label on the delegation setup "activation" step dialog.',
  },
  totalLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.totalLabel',
    defaultMessage: '!!!Total',
    description:
      'Total label on the delegation setup "activation" step dialog.',
  },
  spendingPasswordPlaceholder: {
    id:
      'staking.delegationSetup.activation.step.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  confirmButtonLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.confirmButtonLabel',
    defaultMessage: '!!!Confirm',
    description:
      'Label for continue button on the delegation setup "activation" step dialog.',
  },
  postponeButtonLabel: {
    id: 'staking.delegationSetup.activation.step.dialog.postponeButtonLabel',
    defaultMessage: '!!!I’ll do it later',
    description:
      'Postpone button label on the delegation setup "activation" step dialog.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onActivate: Function,
  onBack: Function,
  onClose: Function,
  stepsList: Array<string>,
};

@observer
export default class DelegationStepsActivationDialog extends Component<Props> {
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
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  submit = () => {
    this.form.submit({
      onSuccess: form => {
        const { spendingPassword } = form.values();
        const data = {
          fees: 12.042481,
          amount: 3,
          total: 15.042481,
          spendingPassword,
        };
        this.props.onActivate(data);
        form.clear();
      },
      onError: () => {},
    });
  };

  handleSubmitOnEnter = submitOnEnter.bind(this, this.submit);

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onBack, onClose, stepsList } = this.props;

    const spendingPasswordField = form.$('spendingPassword');

    const actions = [
      {
        className: 'postponeButton',
        label: intl.formatMessage(messages.postponeButtonLabel),
        onClick: onClose,
      },
      {
        className: 'confirmButton',
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: this.submit,
        primary: true,
      },
    ];

    const dialogClassName = classNames([
      commonStyles.delegationSteps,
      styles.delegationStepsActivationDialogWrapper,
    ]);
    const contentClassName = classNames([commonStyles.content, styles.content]);

    const stepsIndicatorLabel = (
      <FormattedMessage
        {...messages.stepIndicatorLabel}
        values={{
          currentStep: 4,
          totalSteps: stepsList.length,
        }}
      />
    );

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        subtitle={stepsIndicatorLabel}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={dialogClassName}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={commonStyles.delegationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={4}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={contentClassName}>
          <div className={styles.description}>
            <FormattedHTMLMessage {...messages.descriptionLine1} />
            <p>{intl.formatMessage(messages.descriptionLine2)}</p>
          </div>

          <div className={styles.addressWrapper}>
            <p className={styles.label}>
              {intl.formatMessage(messages.addressLabel)}
            </p>
            <p className={styles.addressValue}>
              YbDziZoPjGmJdssagaugyCqUUJVySKBdA1DUHbpYmQd6yTeFQqfrWWKx9gs19MxMbcEskurDMdVX1h32Fi94Nojxp1gvwM
            </p>
          </div>

          <div className={styles.resumeWrapper}>
            <div className={styles.amountWrapper}>
              <p className={styles.label}>
                {intl.formatMessage(messages.amountLabel)}
              </p>
              <p className={styles.amount}>
                3<span> ADA</span>
              </p>
            </div>

            <div className={styles.feesWrapper}>
              <p className={styles.label}>
                {intl.formatMessage(messages.feesLabel)}
              </p>
              <p className={styles.amount}>
                + 12.042481<span> ADA</span>
              </p>
            </div>
          </div>

          <div className={styles.totalWrapper}>
            <p className={styles.label}>
              {intl.formatMessage(messages.totalLabel)}
            </p>
            <p className={styles.amount}>
              15.042481<span> ADA</span>
            </p>
          </div>

          <Input
            className={styles.spendingPassword}
            {...spendingPasswordField.bind()}
            skin={InputSkin}
            error={spendingPasswordField.error}
            onKeyPress={this.handleSubmitOnEnter}
          />
        </div>
      </Dialog>
    );
  }
}

// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedMessage } from 'react-intl';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import styles from './DelegationStepsConfirmationDialog.scss';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import DialogBackButton from '../../widgets/DialogBackButton';
import Dialog from '../../widgets/Dialog';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';

const messages = defineMessages({
  title: {
    id: 'staking.delegationSetup.confirmation.step.dialog.title',
    defaultMessage: '!!!Confirm Delegation',
    description:
      'Title "Confirm Delegation" on the delegation setup "confirmation" step dialog.',
  },
  stepIndicatorLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.stepIndicatorLabel',
    defaultMessage: '!!!STEP {currentStep} OF {totalSteps}',
    description:
      'Step indicator labe on the delegation setup "confirmation" step dialog.',
  },
  description: {
    id: 'staking.delegationSetup.confirmation.step.dialog.description',
    defaultMessage:
      '!!!Confirm your delegation to publish your delegation preferences on Cardano blockchain.',
    description:
      'Description on the delegation setup "confirmation" step dialog.',
  },
  feesLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.feesLabel',
    defaultMessage: '!!!Fees',
    description:
      'Fees label on the delegation setup "confirmation" step dialog.',
  },
  spendingPasswordPlaceholder: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.spendingPasswordPlaceholder',
    defaultMessage: '!!!Password',
    description: 'Placeholder for "spending password"',
  },
  spendingPasswordLabel: {
    id:
      'staking.delegationSetup.confirmation.step.dialog.spendingPasswordLabel',
    defaultMessage: '!!!Spending password',
    description: 'Label for "spending password"',
  },
  confirmButtonLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.confirmButtonLabel',
    defaultMessage: '!!!Confirm',
    description:
      'Label for continue button on the delegation setup "confirmation" step dialog.',
  },
  cancelButtonLabel: {
    id: 'staking.delegationSetup.confirmation.step.dialog.cancelButtonLabel',
    defaultMessage: '!!!Cancel',
    description:
      'Label for "Cancel" button on the delegation setup "confirmation" step dialog.',
  },
});

type State = {
  spendingPasswordValue: string,
};

type Props = {
  isSpendingPasswordSet?: boolean,
  onBack: Function,
  onClose: Function,
  onConfirm: Function,
  stepsList: Array<string>,
};

export default class DelegationStepsConfirmationDialog extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    spendingPasswordValue: '',
  };

  form = new ReactToolboxMobxForm({
    fields: {
      spendingPassword: {
        type: 'password',
        label: this.context.intl.formatMessage(messages.spendingPasswordLabel),
        placeholder: this.context.intl.formatMessage(
          messages.spendingPasswordPlaceholder
        ),
        value: '',
      },
    },
  });

  submit = () => {
    this.form.submit({
      onSuccess: () => {
        const { isSpendingPasswordSet } = this.props;
        const data = {
          fees: 0.172081,
          password: isSpendingPasswordSet
            ? this.state.spendingPasswordValue
            : null,
        };
        this.props.onConfirm(data);
      },
      onError: () => {},
    });
  };

  handlePasswordChange = (value: string) => {
    this.setState({ spendingPasswordValue: value });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { isSpendingPasswordSet, onBack, onClose, stepsList } = this.props;
    const { spendingPasswordValue } = this.state;

    const spendingPasswordField = form.$('spendingPassword');

    const actions = [
      {
        className: 'cancelButton',
        label: intl.formatMessage(messages.cancelButtonLabel),
        onClick: onClose,
      },
      {
        className: 'confirmButton',
        label: intl.formatMessage(messages.confirmButtonLabel),
        onClick: this.submit,
        primary: true,
      },
    ];

    return (
      <Dialog
        title={intl.formatMessage(messages.title)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        className={styles.delegationStepsConfirmationDialogWrapper}
        closeButton={<DialogCloseButton onClose={onClose} />}
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={styles.delegationStepsIndicatorWrapper}>
          <p className={styles.stepIndicatorLabel}>
            <FormattedMessage
              {...messages.stepIndicatorLabel}
              values={{
                currentStep: 3,
                totalSteps: stepsList.length,
              }}
            />
          </p>
          <Stepper
            steps={stepsList}
            activeStep={3}
            skin={StepperSkin}
            labelDisabled
          />
        </div>

        <div className={styles.content}>
          <p className={styles.description}>
            {intl.formatMessage(messages.description)}
          </p>

          <div className={styles.feesWrapper}>
            <p className={styles.feesLabel}>
              {intl.formatMessage(messages.feesLabel)}
            </p>
            <p className={styles.feesAmount}>
              0.172081<span> ADA</span>
            </p>
          </div>

          {isSpendingPasswordSet && (
            <Input
              type="password"
              className={styles.spendingPassword}
              {...spendingPasswordField.bind()}
              skin={InputSkin}
              onChange={value => {
                this.handlePasswordChange(value);
              }}
              value={spendingPasswordValue}
              autoFocus
            />
          )}
        </div>
      </Dialog>
    );
  }
}

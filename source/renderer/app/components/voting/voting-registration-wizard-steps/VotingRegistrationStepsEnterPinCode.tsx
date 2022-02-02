import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import vjf from 'mobx-react-form/lib/validators/VJF';
import PinCode from '../../widgets/forms/PinCode';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  isValidPinCode,
  isValidRepeatPinCode,
} from '../../../utils/validations';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import {
  VOTING_REGISTRATION_PIN_CODE_LENGTH,
  NEXT_VOTING_FUND_NUMBER,
} from '../../../config/votingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsEnter... Remove this comment to see the full error message
import styles from './VotingRegistrationStepsEnterPinCode.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.enterPinCode.step.description',
    defaultMessage:
      '!!!Please enter a PIN for your Fund{nextVotingFundNumber} voting registration. The PIN you set here, and the QR code which you will get in the next step, will be required for you to vote using the Catalyst Voting app on your smartphone.',
    description:
      'Description on the voting registration "enter pin code" step.',
  },
  reminder: {
    id: 'voting.votingRegistration.enterPinCode.step.reminder',
    defaultMessage:
      '!!!<span>It is important to remember your PIN.</span> If you forget your PIN, you will not be able to use this registration for voting, and you will need to repeat the registration process.',
    description: 'Reminder on the voting registration "enter pin code" step.',
  },
  enterPinCodeLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.enterPinCodeLabel',
    defaultMessage: '!!!Enter PIN',
    description:
      'Label for pin code input on the voting registration "enter pin code" step.',
  },
  repeatPinCodeLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.repeatPinCodeLabel',
    defaultMessage: '!!!Repeat PIN',
    description:
      'Label for repeat pin code on the voting registration "enter pin code" step.',
  },
  invalidPinCode: {
    id: 'voting.votingRegistration.enterPinCode.step.errors.invalidPinCode',
    defaultMessage: '!!!Invalid PIN',
    description: 'Error message shown when repeat pin code is invalid.',
  },
  invalidRepeatPinCode: {
    id:
      'voting.votingRegistration.enterPinCode.step.errors.invalidRepeatPinCode',
    defaultMessage: '!!!PIN doesn’t match',
    description: 'Error message shown when repeat pin code is invalid.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "enter pin code" step.',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  stepsList: Array<string>;
  activeStep: number;
  onSetPinCode: (...args: Array<any>) => any;
};

@observer
class VotingRegistrationStepsEnterPinCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm(
    // @ts-ignore ts-migrate(2554) FIXME: Expected 0 arguments, but got 2.
    {
      fields: {
        pinCode: {
          type: 'password',
          value: [],
          validators: [
            ({ field, form }) => {
              const value = field.value ? field.value.join('') : '';
              const repeatPinCodeField = form.$('repeatPinCode');
              const isRepeatPinCodeFieldSet =
                value.length === VOTING_REGISTRATION_PIN_CODE_LENGTH;
              repeatPinCodeField.validate({
                showErrors: isRepeatPinCodeFieldSet,
              });
              return [
                isValidPinCode(value, VOTING_REGISTRATION_PIN_CODE_LENGTH),
                this.context.intl.formatMessage(messages.invalidPinCode),
              ];
            },
          ],
        },
        repeatPinCode: {
          type: 'password',
          value: [],
          validators: [
            ({ field, form }) => {
              const value = field.value ? field.value.join('') : '';
              const pinCode = form.$('pinCode').value
                ? form.$('pinCode').value.join('')
                : '';
              return [
                isValidRepeatPinCode(pinCode, value),
                this.context.intl.formatMessage(messages.invalidRepeatPinCode),
              ];
            },
          ],
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
        const { pinCode } = form.values();
        this.props.onSetPinCode(pinCode.join(''));
      },
    });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onClose, stepsList, activeStep } = this.props;
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const pinCodeField = form.$('pinCode');
    // @ts-ignore ts-migrate(2339) FIXME: Property '$' does not exist on type 'ReactToolboxM... Remove this comment to see the full error message
    const repeatPinCodeField = form.$('repeatPinCode');
    const pinCodeFieldProps = pinCodeField.bind();
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();
    const actions = [
      {
        label: buttonLabel,
        onClick: this.submit,
        // @ts-ignore ts-migrate(2339) FIXME: Property 'isValid' does not exist on type 'ReactTo... Remove this comment to see the full error message
        disabled: !form.isValid,
        primary: true,
      },
    ];
    return (
      <VotingRegistrationDialog
        onClose={() => {
          onClose(true);
        }}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        containerClassName={styles.component}
      >
        <p className={styles.description}>
          <FormattedHTMLMessage
            {...messages.description}
            values={{
              nextVotingFundNumber: NEXT_VOTING_FUND_NUMBER,
            }}
          />
        </p>

        <div className={styles.pinCode}>
          <PinCode
            {...pinCodeFieldProps}
            label={enterPinCodeLabel}
            autoFocus
            onChange={(...args) => pinCodeFieldProps.onChange(...args)}
          />
          <PinCode
            {...repeatPinCodeFieldProps}
            label={repeatPinCodeLabel}
            onChange={(...args) => repeatPinCodeFieldProps.onChange(...args)}
            autoFocus={pinCodeField.isValid && !repeatPinCodeField.isValid}
            disabled={!pinCodeField.isValid}
            error={repeatPinCodeField.error}
          />
        </div>

        <p className={styles.reminder}>
          <FormattedHTMLMessage {...messages.reminder} />
        </p>
      </VotingRegistrationDialog>
    );
  }
}

export default VotingRegistrationStepsEnterPinCode;

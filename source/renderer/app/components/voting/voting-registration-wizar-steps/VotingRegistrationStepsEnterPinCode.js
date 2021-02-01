// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { observer } from 'mobx-react';
import vjf from 'mobx-react-form/lib/validators/VJF';
import PinCode from '../../widgets/forms/PinCode';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  isValidPinCode,
  isValidRepeatPinCode,
} from '../../../utils/validations';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { VOTING_REGISTRATION_PIN_CODE_LENGTH } from '../../../config/votingConfig';
import commonStyles from './VotingRegistrationSteps.scss';
import styles from './VotingRegistrationStepsEnterPinCode.scss';

const messages = defineMessages({
  title: {
    id: 'voting.votingRegistration.enterPinCode.step.title',
    defaultMessage: '!!!Congratulations!',
    description: 'Title on the voting registration "enter pin code" step.',
  },
  subtitle: {
    id: 'voting.votingRegistration.enterPinCode.step.subtitle',
    defaultMessage: '!!!Your registration transaction was confirmed.',
    description: 'Subtitle on the voting registration "enter pin code" step.',
  },
  description: {
    id: 'voting.votingRegistration.enterPinCode.step.description',
    defaultMessage:
      '!!!Please create a voting PIN code. This code will be used to access the Catalyst Voting App and decrypt your QR code.',
    description:
      'Description on the voting registration "enter pin code" step.',
  },
  reminder: {
    id: 'voting.votingRegistration.enterPinCode.step.reminder',
    defaultMessage:
      '!!!<span>You need to remember your PIN Code.</span> If you lose it, you will not be able to proceed with the registration process and you will not be able to access the mobile app.',
    description: 'Reminder on the voting registration "enter pin code" step.',
  },
  enterPinCodeLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.enterPinCodeLabel',
    defaultMessage: '!!!Enter PIN code',
    description:
      'Label for pin code input on the voting registration "enter pin code" step.',
  },
  repeatPinCodeLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.repeatPinCodeLabel',
    defaultMessage: '!!!Repeat PIN code',
    description:
      'Label for repeat pin code on the voting registration "enter pin code" step.',
  },
  invalidPinCode: {
    id: 'voting.votingRegistration.enterPinCode.step.errors.invalidPinCode',
    defaultMessage: '!!!Invalid pin code',
    description: 'Error message shown when repeat pin code is invalid.',
  },
  invalidRepeatPinCode: {
    id:
      'voting.votingRegistration.enterPinCode.step.errors.invalidRepeatPinCode',
    defaultMessage: '!!!PIN does not match',
    description: 'Error message shown when repeat pin code is invalid.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.enterPinCode.step.continueButtonLabel',
    defaultMessage: '!!!Create PIN code',
    description:
      'Label for continue button on the voting registration "enter pin code" step.',
  },
});

type Props = {
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  onSetPinCode: Function,
};

@observer
export default class VotingRegistrationStepsEnterPinCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
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
        const { pinCode } = form.values();
        this.props.onSetPinCode(pinCode.join(''));
      },
    });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;

    const title = intl.formatMessage(messages.title);
    const subtitle = intl.formatMessage(messages.subtitle);
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);

    const pinCodeField = form.$('pinCode');
    const repeatPinCodeField = form.$('repeatPinCode');
    const pinCodeFieldProps = pinCodeField.bind();
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();

    const className = classNames([
      commonStyles.votingRegistrationSteps,
      styles.votingRegistrationStepsEnterPinCodeWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    return (
      <div className={className}>
        <div className={contentClassName}>
          <div className={styles.headingDescription}>
            <p>{title}</p>
            <p>{subtitle}</p>
          </div>
          <hr className={styles.separator} />
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
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
              error={repeatPinCodeField.error}
            />
          </div>

          <p className={styles.reminder}>
            <FormattedHTMLMessage {...messages.reminder} />
          </p>
        </div>
        <Button
          onClick={this.submit}
          skin={ButtonSkin}
          label={buttonLabel}
          disabled={!form.isValid}
        />
      </div>
    );
  }
}

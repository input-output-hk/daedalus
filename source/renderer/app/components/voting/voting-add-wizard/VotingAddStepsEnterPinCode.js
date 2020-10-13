// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import classNames from 'classnames';
import commonStyles from './VotingAddSteps.scss';
import styles from './VotingAddStepsEnterPinCode.scss';
import PinCode from '../../widgets/forms/PinCode';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import {
  isValidPinCode,
  isValidRepeatPinCode,
} from '../../../utils/validations';
import globalMessages from '../../../i18n/global-messages';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';
import { observer } from 'mobx-react';

const messages = defineMessages({
  description: {
    id: 'voting.votingAdd.EnterPinCode.step.description',
    defaultMessage:
      '!!!This voting Pin Code will be used to access the Catalyst Voting App and decrypt your QR code. Do not use the same pin code as the one you use for your phone.',
    description: 'Description on the voting add "enter pin code" step.',
  },
  reminder: {
    id: 'voting.votingAdd.EnterPinCode.step.reminder',
    defaultMessage:
      '!!!Dont forget it! You will need it in order to access the voting app and be able to vote.',
    description: 'Reminder on the voting add "enter pin code" step.',
  },
  enterPinCodeLabel: {
    id: 'voting.votingAdd.EnterPinCode.step.enterPinCodeLabel',
    defaultMessage: '!!!Enter PIN code',
    description:
      'Label for pin code input on the voting add "enter pin code" step.',
  },
  repeatPinCodeLabel: {
    id: 'voting.votingAdd.EnterPinCode.step.repeatPinCodeLabel',
    defaultMessage: '!!!Repeat PIN code',
    description:
      'Label for repeat pin code on the voting add "enter pin code" step.',
  },
  invalidPinCode: {
    id: 'voting.votingAdd.EnterPinCode.step.errors.invalidPinCode',
    defaultMessage: '!!!Invalid pin code',
    description: 'Error message shown when repeat pin code is invalid.',
  },
  invalidRepeatPinCode: {
    id: 'voting.votingAdd.EnterPinCode.step.errors.invalidRepeatPinCode',
    defaultMessage: "!!!Pin doesn't match",
    description: 'Error message shown when repeat pin code is invalid.',
  },
  continueButtonLabel: {
    id: 'voting.votingAdd.EnterPinCode.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting add "enter pin code" step.',
  },
});

type Props = {
  onSetPinCode: Function,
};

@observer
export default class VotingAddStepsEnterPinCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        pinCode: {
          type: 'password',
          value: '',
          validators: [
            ({ field, form }) => {
              const repeatPinCodeField = form.$('repeatPinCode');
              const isRepeatPinCodeFieldSet =
                repeatPinCodeField.value.toString().length > 0;
              repeatPinCodeField.validate({
                showErrors: isRepeatPinCodeFieldSet,
              });

              return [
                isValidPinCode(field.value, 4),
                this.context.intl.formatMessage(messages.invalidPinCode),
              ];
            },
          ],
        },
        repeatPinCode: {
          type: 'password',
          value: '',
          validators: [
            ({ field, form }) => {
              const pinCode = form.$('pinCode').value;
              return [
                isValidRepeatPinCode(pinCode, field.value),
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
        this.props.onSetPinCode(pinCode);
      },
      onError: (error) => {},
    });
  };

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onSetPinCode } = this.props;

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);

    const pinCodeField = form.$('pinCode');
    const repeatPinCodeField = form.$('repeatPinCode');
    const pinCodeFieldProps = pinCodeField.bind();
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();

    const className = classNames([
      commonStyles.votingAddSteps,
      styles.votingAddStepsEnterPinCodeWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    return (
      <div className={className}>
        <div className={contentClassName}>
          <p className={styles.description}>
            <FormattedHTMLMessage {...messages.description} />
          </p>

          <div className={styles.pinCode}>
            <PinCode
              {...pinCodeFieldProps}
              label={enterPinCodeLabel}
              autoFocus={true}
              onChange={(...args) => pinCodeFieldProps.onChange(...args)}
            />
            <PinCode
              {...repeatPinCodeFieldProps}
              label={repeatPinCodeLabel}
              onChange={(...args) => repeatPinCodeFieldProps.onChange(...args)}
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
          disabled={!pinCodeField.isValid || !repeatPinCodeField.isValid}
        />
      </div>
    );
  }
}

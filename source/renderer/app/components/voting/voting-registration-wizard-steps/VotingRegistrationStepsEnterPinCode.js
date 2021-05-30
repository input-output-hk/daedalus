// @flow
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
import { VOTING_REGISTRATION_PIN_CODE_LENGTH } from '../../../config/votingConfig';
import styles from './VotingRegistrationStepsEnterPinCode.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.enterPinCode.step.description',
    defaultMessage:
      '!!!Please enter a PIN for your Fund4 voting registration. The PIN you set here, and the QR code which you will get in the next step, will be required for you to vote using the Catalyst Voting app on your smartphone.',
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
  resetPinCodes: {
    id: 'voting.votingRegistration.enterPinCode.step.resetPinCodes',
    defaultMessage: '!!!Reset',
    description: 'Reset pin codes tooltip.',
  },
});

type Props = {
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  onSetPinCode: Function,
};

type State = {
  selectedPinField: ?string,
  pinCodesVisible: boolean,
  sectionToFocus: ?string,
  isTabClicked: boolean,
};

@observer
export default class VotingRegistrationStepsEnterPinCode extends Component<
  Props,
  State
> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  state = {
    selectedPinField: null,
    pinCodesVisible: false,
    sectionToFocus: null,
    isTabClicked: false,
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

  onResetValues = (type: string) => {
    const { form } = this;
    const pinCodeField = form.$(type);
    pinCodeField.value = [];
  };

  onShowHideValues = () => {
    this.setState((prevState) => ({
      pinCodesVisible: !prevState.pinCodesVisible,
    }));
  };

  handleTabKey = (type: string) => {
    this.setState({
      sectionToFocus: type === 'pinCode' ? 'repeatPinCode' : 'pinCode',
    });
  };

  onChangePinCode = (values: Array<string>, isTab?: boolean) => {
    const { form } = this;
    const pinCodeField = form.$('pinCode');
    const pinCodeFieldProps = pinCodeField.bind();

    this.setState({
      selectedPinField: 'pinCode',
      isTabClicked: !!isTab,
    });
    pinCodeFieldProps.onChange(values);
  };

  onChangeRepeatPinCode = (values: Array<string>, isTab?: boolean) => {
    const { form } = this;
    const repeatPinCodeField = form.$('repeatPinCode');
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();

    this.setState({
      selectedPinField: 'repeatPinCode',
      isTabClicked: !!isTab,
    });
    repeatPinCodeFieldProps.onChange(values);
  };

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
    const { onClose, stepsList, activeStep } = this.props;
    const { selectedPinField, pinCodesVisible, sectionToFocus, isTabClicked } = this.state;

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);
    const resetPinCodesLabel = intl.formatMessage(messages.resetPinCodes);

    const pinCodeField = form.$('pinCode');
    const repeatPinCodeField = form.$('repeatPinCode');
    const pinCodeFieldProps = pinCodeField.bind();
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();

    const actions = [
      {
        label: buttonLabel,
        onClick: this.submit,
        disabled: !form.isValid,
        primary: true,
      },
    ];

    const isRepeatPinCodeAutoFocused =
      pinCodeField.isValid &&
      !repeatPinCodeField.isValid &&
      !repeatPinCodeField.value.length;

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
          <FormattedHTMLMessage {...messages.description} />
        </p>

        <div className={styles.pinCode}>
          <PinCode
            {...pinCodeFieldProps}
            label={enterPinCodeLabel}
            resetLabel={resetPinCodesLabel}
            autoFocus
            onChange={(values, isTab) => this.onChangePinCode(values, isTab)}
            onResetValues={(type: string) => this.onResetValues(type)}
            onShowHideValues={() => this.onShowHideValues()}
            selectedPinField={selectedPinField}
            isResetButtonDisabled={!pinCodeField.value.length}
            pinCodesVisible={pinCodesVisible}
            onTabKey={() => this.handleTabKey('pinCode')}
            sectionToFocus={sectionToFocus}
            isTabClicked={isTabClicked}
          />
          <PinCode
            {...repeatPinCodeFieldProps}
            label={repeatPinCodeLabel}
            resetLabel={resetPinCodesLabel}
            onChange={(values, isTab) => this.onChangeRepeatPinCode(values, isTab)}
            onResetValues={(type: string) => this.onResetValues(type)}
            onShowHideValues={() => this.onShowHideValues()}
            autoFocus={isRepeatPinCodeAutoFocused}
            disabled={
              !pinCodeField.isValid &&
              !repeatPinCodeField.value.length &&
              sectionToFocus !== 'repeatPinCode'
            }
            error={repeatPinCodeField.error}
            selectedPinField={selectedPinField}
            isResetButtonDisabled={!repeatPinCodeField.value.length}
            pinCodesVisible={pinCodesVisible}
            onTabKey={() => this.handleTabKey('repeatPinCode')}
            sectionToFocus={sectionToFocus}
            isTabClicked={isTabClicked}
          />
        </div>

        <p className={styles.reminder}>
          <FormattedHTMLMessage {...messages.reminder} />
        </p>
      </VotingRegistrationDialog>
    );
  }
}

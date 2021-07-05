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
      '!!!Please enter a PIN for your Fund5 voting registration. The PIN you set here, and the QR code which you will get in the next step, will be required for you to vote using the Catalyst Voting app on your smartphone.',
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
  pinFieldDisabledStates: Array<boolean>,
  repeatPinFieldDisabledStates: Array<boolean>,
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
    pinFieldDisabledStates: new Array(VOTING_REGISTRATION_PIN_CODE_LENGTH)
      .fill(true)
      .map((item, index) => index !== 0),
    repeatPinFieldDisabledStates: new Array(VOTING_REGISTRATION_PIN_CODE_LENGTH)
      .fill(true)
      .map((item, index) => index !== 0),
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

  componentDidMount() {
    window.addEventListener('keydown', this.handleWindowTabKeyDown);
  }

  componentWillUnmount() {
    window.removeEventListener('keydown', this.handleWindowTabKeyDown);
  }

  onResetValues = (type: string, focusKey: number) => {
    const { form } = this;
    const pinCodeField = form.$(type);
    pinCodeField.value = [];
    if (type === 'pinCode') {
      this.onUpdatePinFieldDisabledStates(focusKey || null, 0, false, true);
    } else {
      this.onUpdateRepeatPinFieldDisabledStates(focusKey || null, 0, false, true);
    }
  };

  onShowHideValues = () => {
    this.setState((prevState) => ({
      pinCodesVisible: !prevState.pinCodesVisible,
    }));
  };

  onUpdatePinFieldDisabledStates = (
    prevFieldIndex: number | null,
    nextFieldIndex: number,
    resetOtherFields?: boolean,
    fromReset?: boolean
  ) => {
    const { pinFieldDisabledStates } = this.state;
    const disabledStates = [...pinFieldDisabledStates];
    const fieldsLength = VOTING_REGISTRATION_PIN_CODE_LENGTH;
    if (prevFieldIndex !== null && prevFieldIndex >= 0) {
      disabledStates[prevFieldIndex] = !disabledStates[prevFieldIndex];
    }
    if (
      nextFieldIndex !== null &&
      nextFieldIndex >= 0 &&
      nextFieldIndex < fieldsLength
    ) {
      disabledStates[nextFieldIndex] = !disabledStates[nextFieldIndex];
    }
    if (fromReset) {
      disabledStates.fill(true, 1, disabledStates.length);
    }
    if (resetOtherFields) {
      disabledStates.fill(true);
    }
    this.setState({
      pinFieldDisabledStates: disabledStates,
    });
  };

  onUpdateRepeatPinFieldDisabledStates = (
    prevFieldIndex: number | null,
    nextFieldIndex: number,
    resetOtherFields?: boolean,
    fromReset?: boolean
  ) => {
    const { repeatPinFieldDisabledStates } = this.state;
    const disabledStates = [...repeatPinFieldDisabledStates];
    const fieldsLength = VOTING_REGISTRATION_PIN_CODE_LENGTH;
    if (prevFieldIndex !== null && prevFieldIndex >= 0) {
      disabledStates[prevFieldIndex] = !disabledStates[prevFieldIndex];
    }
    if (
      nextFieldIndex !== null &&
      nextFieldIndex >= 0 &&
      nextFieldIndex < fieldsLength
    ) {
      disabledStates[nextFieldIndex] = !disabledStates[nextFieldIndex];
    }
    if (fromReset) {
      disabledStates.fill(true, 1, disabledStates.length);
    }
    if (resetOtherFields) {
      disabledStates.fill(true);
    }
    this.setState({
      repeatPinFieldDisabledStates: disabledStates,
    });
  };

  handleTabKey = (type: string, tabClicked?: boolean) => {
    this.setState({
      sectionToFocus: type,
      isTabClicked: !!tabClicked,
    });
  };

  handleWindowTabKeyDown = (event: {
    key: string,
    target: { nodeName: string, name: string },
  }) => {
    const { key, target } = event;
    const { nodeName, name } = target;
    if (key === 'Tab' && nodeName === 'BUTTON') {
      this.handleTabKey('pinCode', true);
    } else if (
      key === 'Tab' &&
      nodeName === 'INPUT' &&
      name === 'repeatPinCode'
    ) {
      this.handleTabKey('continueButton', true);
    }
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
    const {
      selectedPinField,
      pinCodesVisible,
      sectionToFocus,
      isTabClicked,
      pinFieldDisabledStates,
      repeatPinFieldDisabledStates,
    } = this.state;

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);
    const resetPinCodesLabel = intl.formatMessage(messages.resetPinCodes);

    const pinCodeField = form.$('pinCode');
    const repeatPinCodeField = form.$('repeatPinCode');
    const pinCodeFieldProps = pinCodeField.bind();
    const repeatPinCodeFieldProps = repeatPinCodeField.bind();

    const pinCodeFieldsLength = VOTING_REGISTRATION_PIN_CODE_LENGTH;

    const emptyRepeatFieldIndex = repeatPinCodeField.value.findIndex(
      (item) => !item
    );
    const hasError =
      repeatPinCodeField.value.length === pinCodeFieldsLength &&
      emptyRepeatFieldIndex === -1 &&
      repeatPinCodeField.error;

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

    const pinCodeActive = !!pinFieldDisabledStates.filter((item) => !item)
      .length;
    const repeatPinCodeActive = !!repeatPinFieldDisabledStates.filter(
      (item) => !item
    ).length;

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
            onResetValues={(type: string, focusKey: number) =>
              this.onResetValues(type, focusKey)
            }
            onShowHideValues={() => this.onShowHideValues()}
            onUpdateFieldDisabledStates={(
              prevFieldIndex: number,
              nextFieldIndex: number,
              resetOtherFields?: boolean,
              updateOtherFields?: boolean
            ) => {
              this.onUpdatePinFieldDisabledStates(
                prevFieldIndex,
                nextFieldIndex
              );
              if (resetOtherFields) {
                this.onUpdateRepeatPinFieldDisabledStates(
                  null,
                  null,
                  resetOtherFields
                )
              }
              if (updateOtherFields && !repeatPinCodeActive) {
                this.onUpdateRepeatPinFieldDisabledStates(
                  null,
                  repeatPinCodeField.value.length
                )
              }
            }
            }
            selectedPinField={selectedPinField}
            isResetButtonDisabled={!pinCodeField.value.length}
            pinCodesVisible={pinCodesVisible}
            onTabKey={() => this.handleTabKey('repeatPinCode', true)}
            sectionToFocus={sectionToFocus}
            isTabClicked={isTabClicked}
            disabled={
              (!isTabClicked && form.isValid && !pinCodeActive) ||
              (pinCodeField.isValid &&
                !!repeatPinCodeField.value.length &&
                (sectionToFocus === 'repeatPinCode' ||
                  sectionToFocus === 'continueButton'))
            }
            pinFieldDisabledStates={pinFieldDisabledStates}
          />
          <PinCode
            {...repeatPinCodeFieldProps}
            label={repeatPinCodeLabel}
            resetLabel={resetPinCodesLabel}
            onChange={(values, isTab) =>
              this.onChangeRepeatPinCode(values, isTab)
            }
            onResetValues={(type: string, focusKey: number) =>
              this.onResetValues(type, focusKey)
            }
            onShowHideValues={() => this.onShowHideValues()}
            onUpdateFieldDisabledStates={(
              prevFieldIndex: number,
              nextFieldIndex: number,
              resetOtherFields?: boolean,
              updateOtherFields?: boolean
            ) => {
              this.onUpdateRepeatPinFieldDisabledStates(
                prevFieldIndex,
                nextFieldIndex
              );
              if (resetOtherFields) {
                this.onUpdatePinFieldDisabledStates(
                  null,
                  null,
                  resetOtherFields
                )
              }
              if (updateOtherFields && !pinCodeActive) {
                this.onUpdatePinFieldDisabledStates(
                  null,
                  repeatPinCodeField.value.length
                )
              }
            }
            }
            autoFocus={isRepeatPinCodeAutoFocused}
            error={hasError}
            selectedPinField={selectedPinField}
            isResetButtonDisabled={!repeatPinCodeField.value.length}
            pinCodesVisible={pinCodesVisible}
            onTabKey={() => this.handleTabKey('continueButton', true)}
            sectionToFocus={sectionToFocus}
            isTabClicked={isTabClicked}
            disabled={
              (!isTabClicked && form.isValid && !repeatPinCodeActive) ||
              (isTabClicked &&
                sectionToFocus === 'pinCode' &&
                repeatPinCodeField.isValid) ||
              (!isTabClicked &&
                sectionToFocus === 'pinCode' &&
                !repeatPinCodeField.isValid) ||
              (!pinCodeField.isValid &&
                !repeatPinCodeField.value.length &&
                sectionToFocus !== 'repeatPinCode' && pinCodeActive) ||
              (pinCodeField.isValid &&
                !repeatPinCodeField.value.length &&
                selectedPinField !== 'repeatPinCode' && !repeatPinCodeActive) ||
              (sectionToFocus === 'continueButton')
            }
            repeatPinFieldDisabledStates={repeatPinFieldDisabledStates}
          />
        </div>

        <p className={styles.reminder}>
          <FormattedHTMLMessage {...messages.reminder} />
        </p>
      </VotingRegistrationDialog>
    );
  }
}

'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const react_intl_1 = require('react-intl');
const mobx_react_1 = require('mobx-react');
const VJF_1 = __importDefault(require('mobx-react-form/lib/validators/VJF'));
const PinCode_1 = __importDefault(require('../../widgets/forms/PinCode'));
const ReactToolboxMobxForm_1 = __importDefault(
  require('../../../utils/ReactToolboxMobxForm')
);
const validations_1 = require('../../../utils/validations');
const timingConfig_1 = require('../../../config/timingConfig');
const votingConfig_1 = require('../../../config/votingConfig');
const VotingRegistrationStepsEnterPinCode_scss_1 = __importDefault(
  require('./VotingRegistrationStepsEnterPinCode.scss')
);
const VotingRegistrationDialog_1 = __importDefault(
  require('./widgets/VotingRegistrationDialog')
);
const messages = (0, react_intl_1.defineMessages)({
  description: {
    id: 'voting.votingRegistration.enterPinCode.step.description',
    defaultMessage:
      '!!!Set a PIN for your voting registration. You will need this PIN and the QR code you will get in the next step to vote in the Catalyst Voting app.',
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
let VotingRegistrationStepsEnterPinCode = class VotingRegistrationStepsEnterPinCode extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  form = new ReactToolboxMobxForm_1.default(
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
                value.length ===
                votingConfig_1.VOTING_REGISTRATION_PIN_CODE_LENGTH;
              repeatPinCodeField.validate({
                showErrors: isRepeatPinCodeFieldSet,
              });
              return [
                (0, validations_1.isValidPinCode)(
                  value,
                  votingConfig_1.VOTING_REGISTRATION_PIN_CODE_LENGTH
                ),
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
                (0, validations_1.isValidRepeatPinCode)(pinCode, value),
                this.context.intl.formatMessage(messages.invalidRepeatPinCode),
              ];
            },
          ],
        },
      },
    },
    {
      plugins: {
        vjf: (0, VJF_1.default)(),
      },
      options: {
        validateOnChange: true,
        validationDebounceWait: timingConfig_1.FORM_VALIDATION_DEBOUNCE_WAIT,
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
    const { onClose, stepsList, activeStep } = this.props;
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const enterPinCodeLabel = intl.formatMessage(messages.enterPinCodeLabel);
    const repeatPinCodeLabel = intl.formatMessage(messages.repeatPinCodeLabel);
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
    return react_1.default.createElement(
      VotingRegistrationDialog_1.default,
      {
        onClose: () => {
          onClose(true);
        },
        stepsList: stepsList,
        activeStep: activeStep,
        actions: actions,
        containerClassName:
          VotingRegistrationStepsEnterPinCode_scss_1.default.component,
      },
      react_1.default.createElement(
        'p',
        {
          className:
            VotingRegistrationStepsEnterPinCode_scss_1.default.description,
        },
        intl.formatMessage(messages.description)
      ),
      react_1.default.createElement(
        'div',
        {
          className: VotingRegistrationStepsEnterPinCode_scss_1.default.pinCode,
        },
        react_1.default.createElement(PinCode_1.default, {
          ...pinCodeFieldProps,
          label: enterPinCodeLabel,
          autoFocus: true,
          onChange: (...args) => pinCodeFieldProps.onChange(...args),
        }),
        react_1.default.createElement(PinCode_1.default, {
          ...repeatPinCodeFieldProps,
          label: repeatPinCodeLabel,
          onChange: (...args) => repeatPinCodeFieldProps.onChange(...args),
          autoFocus: pinCodeField.isValid && !repeatPinCodeField.isValid,
          disabled: !pinCodeField.isValid,
          error: repeatPinCodeField.error,
        })
      ),
      react_1.default.createElement(
        'p',
        {
          className:
            VotingRegistrationStepsEnterPinCode_scss_1.default.reminder,
        },
        react_1.default.createElement(react_intl_1.FormattedHTMLMessage, {
          ...messages.reminder,
        })
      )
    );
  }
};
VotingRegistrationStepsEnterPinCode = __decorate(
  [mobx_react_1.observer],
  VotingRegistrationStepsEnterPinCode
);
exports.default = VotingRegistrationStepsEnterPinCode;
//# sourceMappingURL=VotingRegistrationStepsEnterPinCode.js.map

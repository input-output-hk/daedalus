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
const classnames_1 = __importDefault(require('classnames'));
const mobx_react_1 = require('mobx-react');
const Button_1 = require('@react-polymorph/components/Button');
const Checkbox_1 = require('@react-polymorph/components/Checkbox');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const CheckboxSkin_1 = require('@react-polymorph/skins/simple/CheckboxSkin');
const react_intl_1 = require('react-intl');
const TermsOfUseText_1 = __importDefault(require('./TermsOfUseText'));
const TermsOfUseForm_scss_1 = __importDefault(require('./TermsOfUseForm.scss'));
const messages = (0, react_intl_1.defineMessages)({
  checkboxLabel: {
    id: 'profile.termsOfUse.checkboxLabel',
    defaultMessage: '!!!I agree with terms of service',
    description: 'Label for the "I agree with terms of service" checkbox.',
  },
  checkboxLabelWithDisclaimer: {
    id: 'profile.termsOfUse.checkboxLabelWithDisclaimer',
    defaultMessage:
      '!!!I understand that the terms of use are only available in English and agree to the terms of use',
    description:
      'Label for the "I agree with terms of service" checkbox when terms of use are not translated.',
  },
  submitLabel: {
    id: 'profile.termsOfUse.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Terms of service" form submit button.',
  },
});
let TermsOfUseForm = class TermsOfUseForm extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  state = {
    areTermsOfUseAccepted: false,
  };
  toggleAcceptance = () => {
    this.setState((prevState) => ({
      areTermsOfUseAccepted: !prevState.areTermsOfUseAccepted,
    }));
  };
  submit = () => {
    this.props.onSubmit();
  };
  render() {
    const { intl } = this.context;
    const {
      isSubmitting,
      error,
      localizedTermsOfUse,
      onOpenExternalLink,
    } = this.props;
    const { areTermsOfUseAccepted } = this.state;
    const buttonClasses = (0, classnames_1.default)([
      'primary',
      isSubmitting
        ? TermsOfUseForm_scss_1.default.submitButtonSpinning
        : TermsOfUseForm_scss_1.default.submitButton,
    ]);
    return react_1.default.createElement(
      'div',
      { className: TermsOfUseForm_scss_1.default.component },
      react_1.default.createElement(
        'div',
        { className: TermsOfUseForm_scss_1.default.centeredBox },
        react_1.default.createElement(TermsOfUseText_1.default, {
          localizedTermsOfUse: localizedTermsOfUse,
          onOpenExternalLink: onOpenExternalLink,
        }),
        react_1.default.createElement(
          'div',
          { className: TermsOfUseForm_scss_1.default.checkbox },
          react_1.default.createElement(Checkbox_1.Checkbox, {
            label: intl.formatMessage(messages.checkboxLabel),
            onChange: this.toggleAcceptance,
            checked: areTermsOfUseAccepted,
            skin: CheckboxSkin_1.CheckboxSkin,
          })
        ),
        error &&
          react_1.default.createElement(
            'p',
            { className: TermsOfUseForm_scss_1.default.error },
            intl.formatMessage(error)
          ),
        react_1.default.createElement(Button_1.Button, {
          className: buttonClasses,
          label: intl.formatMessage(messages.submitLabel),
          onClick: this.submit,
          disabled: !areTermsOfUseAccepted,
          skin: ButtonSkin_1.ButtonSkin,
        })
      )
    );
  }
};
TermsOfUseForm = __decorate([mobx_react_1.observer], TermsOfUseForm);
exports.default = TermsOfUseForm;
//# sourceMappingURL=TermsOfUseForm.js.map

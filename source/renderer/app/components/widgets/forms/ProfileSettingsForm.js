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
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const Select_1 = require('@react-polymorph/components/Select');
const ButtonSpinnerSkin_1 = require('@react-polymorph/skins/simple/ButtonSpinnerSkin');
const SelectSkin_1 = require('@react-polymorph/skins/simple/SelectSkin');
const react_intl_1 = require('react-intl');
const ProfileSettingsForm_scss_1 = __importDefault(
  require('./ProfileSettingsForm.scss')
);
const profileConfig_1 = require('../../../config/profileConfig');
const messages = (0, react_intl_1.defineMessages)({
  localeSelectLabel: {
    id: 'profile.settings.languageSelect.label',
    defaultMessage: '!!!Language',
    description: 'Label for the language select.',
  },
  numberFormatSelectLabel: {
    id: 'profile.settings.numberSelect.label',
    defaultMessage: '!!!Number format',
    description: 'Label for the number select.',
  },
  dateFormatSelectLabel: {
    id: 'profile.settings.dateSelect.label',
    defaultMessage: '!!!Date format',
    description: 'Label for the date select.',
  },
  timeFormatSelectLabel: {
    id: 'profile.settings.timeSelect.label',
    defaultMessage: '!!!Time format',
    description: 'Label for the time select.',
  },
  submitLabel: {
    id: 'profile.settings.submitLabel',
    defaultMessage: '!!!Continue',
    description: 'Label for the "Language select" form submit button.',
  },
});
let ProfileSettingsForm = class ProfileSettingsForm extends react_1.Component {
  static defaultProps = {
    onChangeItem: () => {},
  };
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  get locale() {
    const { props, context } = this;
    const options = profileConfig_1.LANGUAGE_OPTIONS.map((language) => ({
      value: language.value,
      label: context.intl.formatMessage(language.label),
    }));
    const value = props.currentLocale;
    return {
      value,
      options,
    };
  }
  get numberFormat() {
    return {
      options: profileConfig_1.NUMBER_OPTIONS,
      value: this.props.currentNumberFormat,
    };
  }
  get dateFormat() {
    const { currentLocale, currentDateFormat } = this.props;
    return {
      options:
        currentLocale === 'en-US'
          ? profileConfig_1.DATE_ENGLISH_OPTIONS
          : profileConfig_1.DATE_JAPANESE_OPTIONS,
      value: currentDateFormat,
    };
  }
  get timeFormat() {
    return {
      options: profileConfig_1.TIME_OPTIONS,
      value: this.props.currentTimeFormat,
    };
  }
  getSelect = (id) => {
    const { formatMessage } = this.context.intl;
    const { onChangeItem } = this.props;
    const { value, options } = this[id];
    return react_1.default.createElement(Select_1.Select, {
      label: formatMessage(messages[`${id}SelectLabel`]),
      value: value,
      options: options,
      onChange: (v) => onChangeItem(id, v),
      skin: SelectSkin_1.SelectSkin,
      className: ProfileSettingsForm_scss_1.default.select,
      key: id,
      optionHeight: 50,
    });
  };
  render() {
    const { error, onSubmit, isSubmitting } = this.props;
    const { formatMessage } = this.context.intl;
    const componentClassNames = (0, classnames_1.default)([
      ProfileSettingsForm_scss_1.default.component,
      'general',
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClassNames },
      profileConfig_1.PROFILE_SETTINGS.map((param) => this.getSelect(param)),
      error &&
        react_1.default.createElement(
          'p',
          { className: ProfileSettingsForm_scss_1.default.error },
          error
        ),
      onSubmit &&
        react_1.default.createElement(Button_1.Button, {
          className: (0, classnames_1.default)([
            'primary',
            ProfileSettingsForm_scss_1.default.submitButton,
          ]),
          label: formatMessage(messages.submitLabel),
          skin: ButtonSpinnerSkin_1.ButtonSpinnerSkin,
          loading: isSubmitting,
          onClick: onSubmit,
        })
    );
  }
};
ProfileSettingsForm = __decorate([mobx_react_1.observer], ProfileSettingsForm);
exports.default = ProfileSettingsForm;
//# sourceMappingURL=ProfileSettingsForm.js.map

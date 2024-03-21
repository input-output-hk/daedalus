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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const react_datetime_1 = __importDefault(require('react-datetime'));
const react_intl_1 = require('react-intl');
const moment_1 = __importDefault(require('moment'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const TinyButton_1 = __importDefault(require('./TinyButton'));
const TinyInput_1 = __importDefault(require('./TinyInput'));
const TinyDatePicker_scss_1 = __importDefault(require('./TinyDatePicker.scss'));
class TinyDatePicker extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  static defaultProps = {
    onReset: () => null,
    onChange: () => null,
    locale: 'en-us',
  };
  render() {
    const {
      onReset,
      onChange,
      isValidDate,
      dateFormat,
      value,
      label,
      placeholder,
      innerLabelPrefix,
      innerValue,
      useReadMode,
      error,
      ...rest
    } = this.props;
    /* eslint-disable */
    return react_1.default.createElement(
      PopOver_1.PopOver,
      {
        interactive: true,
        arrow: false,
        placement: 'auto',
        duration: 0,
        trigger: 'click',
        content: react_1.default.createElement(
          'div',
          { className: TinyDatePicker_scss_1.default.datePicker },
          react_1.default.createElement(react_datetime_1.default, {
            open: true,
            input: false,
            dateFormat: dateFormat,
            timeFormat: false,
            value: value ? (0, moment_1.default)(value).toDate() : null,
            isValidDate: isValidDate,
            onChange: (selectedDate) => {
              if (typeof selectedDate === 'string') {
                if (!selectedDate) {
                  onChange && onChange(selectedDate);
                }
              } else {
                onChange && onChange(selectedDate.format('YYYY-MM-DD'));
              }
            },
            ...rest,
          }),
          react_1.default.createElement(TinyButton_1.default, {
            containerClassName: TinyDatePicker_scss_1.default.resetButton,
            onClick: onReset,
            label: this.context.intl.formatMessage(
              global_messages_1.default.reset
            ),
            disabled: value == null || value === '',
          })
        ),
      },
      react_1.default.createElement(TinyInput_1.default, {
        autoFocus: false,
        value: value ? (0, moment_1.default)(value).format(dateFormat) : '',
        label: label,
        placeholder: placeholder,
        innerLabelPrefix: innerLabelPrefix,
        onChange: (value, evt) => console.log(evt),
        useReadMode: useReadMode,
        innerValue: innerValue,
        error: error,
      })
    );
  }
}
exports.default = TinyDatePicker;
//# sourceMappingURL=TinyDatePicker.js.map

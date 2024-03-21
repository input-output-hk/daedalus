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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const react_intl_1 = require('react-intl');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const Select_1 = require('@react-polymorph/components/Select');
const SelectSkin_1 = require('@react-polymorph/skins/simple/SelectSkin');
const InlineEditingDropdown_scss_1 = __importDefault(
  require('./InlineEditingDropdown.scss')
);
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/questio... Remove this comment to see the full error message
const question_mark_inline_svg_1 = __importDefault(
  require('../../../assets/images/question-mark.inline.svg')
);
const messages = (0, react_intl_1.defineMessages)({
  changesSaved: {
    id: 'inline.editing.dropdown.changesSaved',
    defaultMessage: '!!!Your changes have been saved',
    description:
      'Message "Your changes have been saved" for inline editing (eg. on Wallet Settings page).',
  },
});
let InlineEditingDropdown = class InlineEditingDropdown extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  onChange = (value) => {
    this.props.onStartEditing();
    this.props.onSubmit(value);
    this.props.onStopEditing();
  };
  render() {
    const { intl } = this.context;
    const {
      className,
      isActive,
      label,
      tooltip,
      options,
      value,
      successfullyUpdated,
    } = this.props;
    const componentClasses = (0, classnames_1.default)([
      className,
      InlineEditingDropdown_scss_1.default.component,
    ]);
    const dropdownStyles = (0, classnames_1.default)([
      successfullyUpdated ? 'dropdown_animateSuccess' : null,
    ]);
    const labelText = [
      label,
      !!tooltip &&
        react_1.default.createElement(
          PopOver_1.PopOver,
          { content: tooltip, key: 'tooltip' },
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: question_mark_inline_svg_1.default,
            className: InlineEditingDropdown_scss_1.default.questionMarkIcon,
          })
        ),
    ];
    return react_1.default.createElement(
      'div',
      { className: componentClasses },
      react_1.default.createElement(Select_1.Select, {
        className: dropdownStyles,
        label: labelText,
        options: options,
        value: value,
        onChange: this.onChange,
        disabled: !isActive,
        skin: SelectSkin_1.SelectSkin,
      }),
      successfullyUpdated &&
        react_1.default.createElement(
          'div',
          { className: InlineEditingDropdown_scss_1.default.savingResultLabel },
          intl.formatMessage(messages.changesSaved)
        )
    );
  }
};
InlineEditingDropdown = __decorate(
  [mobx_react_1.observer],
  InlineEditingDropdown
);
exports.default = InlineEditingDropdown;
//# sourceMappingURL=InlineEditingDropdown.js.map

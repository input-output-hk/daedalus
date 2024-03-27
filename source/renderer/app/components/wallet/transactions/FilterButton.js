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
const react_intl_1 = require('react-intl');
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const TinyButton_1 = __importDefault(require('../../widgets/forms/TinyButton'));
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/filter-... Remove this comment to see the full error message
const filter_dis_ic_inline_svg_1 = __importDefault(
  require('../../../assets/images/filter-dis-ic.inline.svg')
);
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const FilterButton_scss_1 = __importDefault(require('./FilterButton.scss'));
let FilterButton = class FilterButton extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { intl } = this.context;
    const { numberOfFilterDimensionsApplied, onClick, disabled } = this.props;
    const buttonLabel = react_1.default.createElement(
      react_1.default.Fragment,
      null,
      react_1.default.createElement(
        'div',
        { className: FilterButton_scss_1.default.actionLabel },
        intl.formatMessage(global_messages_1.default.filter),
        numberOfFilterDimensionsApplied > 0 &&
          react_1.default.createElement(
            'span',
            { className: FilterButton_scss_1.default.numberIndicator },
            '(',
            numberOfFilterDimensionsApplied,
            ')'
          )
      ),
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: filter_dis_ic_inline_svg_1.default,
        className: FilterButton_scss_1.default.filterIcon,
      })
    );
    const buttonClasses = (0, classnames_1.default)([
      'primary',
      FilterButton_scss_1.default.actionButton,
    ]);
    return react_1.default.createElement(
      'div',
      { className: FilterButton_scss_1.default.component },
      react_1.default.createElement(TinyButton_1.default, {
        className: buttonClasses,
        label: buttonLabel,
        loading: false,
        onClick: onClick,
        disabled: disabled,
      })
    );
  }
};
FilterButton = __decorate([mobx_react_1.observer], FilterButton);
exports.default = FilterButton;
//# sourceMappingURL=FilterButton.js.map

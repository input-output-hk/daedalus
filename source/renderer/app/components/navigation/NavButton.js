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
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const NavButton_scss_1 = __importDefault(require('./NavButton.scss'));
let NavButton = class NavButton extends react_1.Component {
  render() {
    const { isActive, icon, onClick, className, hasNotification } = this.props;
    const componentClasses = (0, classnames_1.default)([
      className,
      NavButton_scss_1.default.component,
      isActive
        ? NavButton_scss_1.default.active
        : NavButton_scss_1.default.normal,
      hasNotification ? NavButton_scss_1.default.hasNotification : null,
    ]);
    const iconClasses = (0, classnames_1.default)([
      NavButton_scss_1.default.icon,
      isActive
        ? NavButton_scss_1.default.activeIcon
        : NavButton_scss_1.default.normalIcon,
    ]);
    return react_1.default.createElement(
      'button',
      { className: componentClasses, onClick: onClick },
      react_1.default.createElement(
        'div',
        { className: NavButton_scss_1.default.container },
        icon &&
          react_1.default.createElement(react_svg_inline_1.default, {
            svg: icon,
            className: iconClasses,
          }),
        react_1.default.createElement(
          'span',
          { className: NavButton_scss_1.default.label },
          this.props.label
        )
      )
    );
  }
};
NavButton = __decorate([mobx_react_1.observer], NavButton);
exports.default = NavButton;
//# sourceMappingURL=NavButton.js.map

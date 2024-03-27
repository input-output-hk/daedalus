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
const react_1 = __importStar(require('react'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const classnames_1 = __importDefault(require('classnames'));
const BigButtonForDialogs_scss_1 = __importDefault(
  require('./BigButtonForDialogs.scss')
);
class BigButtonForDialogs extends react_1.Component {
  render() {
    const {
      label,
      description,
      icon,
      onClick,
      isDisabled = false,
      className,
    } = this.props;
    const componentClasses = (0, classnames_1.default)([
      className,
      BigButtonForDialogs_scss_1.default.component,
      isDisabled ? BigButtonForDialogs_scss_1.default.disabled : null,
    ]);
    return react_1.default.createElement(
      'button',
      {
        className: componentClasses,
        onClick: () => !isDisabled && onClick && onClick(),
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: icon,
        className: BigButtonForDialogs_scss_1.default.icon,
      }),
      react_1.default.createElement(
        'div',
        { className: BigButtonForDialogs_scss_1.default.label },
        label
      ),
      react_1.default.createElement(
        'div',
        { className: BigButtonForDialogs_scss_1.default.description },
        description
      )
    );
  }
}
exports.default = BigButtonForDialogs;
//# sourceMappingURL=BigButtonForDialogs.js.map

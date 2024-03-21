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
const classnames_1 = __importDefault(require('classnames'));
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const spinner_dark_big_inline_svg_1 = __importDefault(
  require('../../assets/images/spinner-dark-big.inline.svg')
);
const spinner_dark_inline_svg_1 = __importDefault(
  require('../../assets/images/spinner-dark.inline.svg')
);
const LoadingSpinner_scss_1 = __importDefault(require('./LoadingSpinner.scss'));
class LoadingSpinner extends react_1.Component {
  static defaultProps = {
    big: false,
    medium: false,
  };
  root;
  render() {
    const { big, medium, className } = this.props;
    const icon = big
      ? spinner_dark_big_inline_svg_1.default
      : spinner_dark_inline_svg_1.default;
    const componentClasses = (0, classnames_1.default)([
      LoadingSpinner_scss_1.default.component,
      big ? LoadingSpinner_scss_1.default.big : null,
      medium ? LoadingSpinner_scss_1.default.medium : null,
      !big && !medium ? LoadingSpinner_scss_1.default.small : null,
      className || null,
    ]);
    return react_1.default.createElement(
      'div',
      {
        className: componentClasses,
        ref: (div) => {
          this.root = div;
        },
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: icon,
        className: LoadingSpinner_scss_1.default.icon,
      })
    );
  }
}
exports.default = LoadingSpinner;
//# sourceMappingURL=LoadingSpinner.js.map

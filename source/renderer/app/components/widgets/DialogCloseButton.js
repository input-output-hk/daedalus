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
const close_cross_inline_svg_1 = __importDefault(
  require('../../assets/images/close-cross.inline.svg')
);
const DialogCloseButton_scss_1 = __importDefault(
  require('./DialogCloseButton.scss')
);
class DialogCloseButton extends react_1.Component {
  render() {
    const { onClose, icon, disabled, className } = this.props;
    const buttonClass = !className ? '' : className;
    return react_1.default.createElement(
      'button',
      {
        onClick: onClose != null ? onClose : () => {},
        className: !disabled
          ? `${DialogCloseButton_scss_1.default.component} ${buttonClass}`
          : `${DialogCloseButton_scss_1.default.disabled} ${buttonClass}`,
        tabIndex: -1,
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: icon || close_cross_inline_svg_1.default,
      })
    );
  }
}
exports.default = DialogCloseButton;
//# sourceMappingURL=DialogCloseButton.js.map

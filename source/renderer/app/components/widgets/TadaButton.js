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
const TadaButton_scss_1 = __importDefault(require('./TadaButton.scss'));
const tada_green_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/tada-green-ic.inline.svg')
);
class TadaButton extends react_1.Component {
  render() {
    const { onClick, iconClass, shouldAnimate } = this.props;
    const componentClasses = (0, classnames_1.default)([
      TadaButton_scss_1.default.component,
      shouldAnimate ? TadaButton_scss_1.default.animate : null,
      iconClass,
    ]);
    return react_1.default.createElement(
      'button',
      { className: componentClasses, onClick: onClick },
      react_1.default.createElement(react_svg_inline_1.default, {
        className: TadaButton_scss_1.default.icon,
        svg: tada_green_ic_inline_svg_1.default,
      })
    );
  }
}
exports.default = TadaButton;
//# sourceMappingURL=TadaButton.js.map

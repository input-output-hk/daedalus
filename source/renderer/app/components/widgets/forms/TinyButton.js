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
const Button_1 = require('@react-polymorph/components/Button');
const ButtonSkin_1 = require('@react-polymorph/skins/simple/ButtonSkin');
const API_1 = require('@react-polymorph/themes/API');
const TinyButton_scss_1 = __importDefault(require('./TinyButton.scss'));
class TinyButton extends react_1.Component {
  render() {
    const { containerClassName, loading, ...buttonProps } = this.props;
    const componentClassnames = (0, classnames_1.default)([
      TinyButton_scss_1.default.component,
      containerClassName,
    ]);
    return react_1.default.createElement(
      'div',
      { className: componentClassnames },
      react_1.default.createElement(Button_1.Button, {
        themeId: API_1.IDENTIFIERS.BUTTON,
        skin: ButtonSkin_1.ButtonSkin,
        loading: loading,
        ...buttonProps,
      })
    );
  }
}
exports.default = TinyButton;
//# sourceMappingURL=TinyButton.js.map

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
const lodash_1 = require('lodash');
const Button_1 = require('@react-polymorph/components/Button');
const Link_1 = require('@react-polymorph/components/Link');
const LinkSkin_1 = require('@react-polymorph/skins/simple/LinkSkin');
const ButtonLink_scss_1 = __importDefault(require('./ButtonLink.scss'));
class ButtonLink extends react_1.Component {
  render() {
    const { label, linkProps } = this.props;
    const linkLabelClasses = (0, lodash_1.get)(linkProps, 'className', null);
    const linkLabel = react_1.default.createElement(Link_1.Link, {
      label: label,
      isUnderlined: (0, lodash_1.get)(linkProps, 'isUnderlined', false),
      underlineOnHover: (0, lodash_1.get)(linkProps, 'underlineOnHover', false),
      hasIconBefore: (0, lodash_1.get)(linkProps, 'hasIconBefore', true),
      hasIconAfter: (0, lodash_1.get)(linkProps, 'hasIconAfter', false),
      className: linkLabelClasses,
      skin: LinkSkin_1.LinkSkin,
      themeOverrides: ButtonLink_scss_1.default,
    });
    return react_1.default.createElement(Button_1.Button, {
      ...this.props,
      label: linkLabel,
    });
  }
}
exports.default = ButtonLink;
//# sourceMappingURL=ButtonLink.js.map

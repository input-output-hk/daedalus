'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.ExternalLinkButton = void 0;
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const Button_1 = require('@react-polymorph/components/Button');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const external_link_ic_inline_svg_1 = __importDefault(
  require('../../assets/images/external-link-ic.inline.svg')
);
const ExternalLinkButton_scss_1 = __importDefault(
  require('./ExternalLinkButton.scss')
);
function ExternalLinkButton({ label, onClick }) {
  const buttonStyles = (0, classnames_1.default)([
    'flat',
    ExternalLinkButton_scss_1.default.overrideButton,
  ]);
  return react_1.default.createElement(Button_1.Button, {
    label: react_1.default.createElement(
      'div',
      { className: ExternalLinkButton_scss_1.default.labelBlock },
      label,
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: external_link_ic_inline_svg_1.default,
        className: ExternalLinkButton_scss_1.default.externalLinkIcon,
      })
    ),
    className: buttonStyles,
    onClick: onClick,
  });
}
exports.ExternalLinkButton = ExternalLinkButton;
//# sourceMappingURL=ExternalLinkButton.js.map

'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.ClearButton = void 0;
// @ts-nocheck
const react_1 = __importDefault(require('react'));
const PopOver_1 = require('@react-polymorph/components/PopOver');
const react_svg_inline_1 = __importDefault(require('react-svg-inline'));
const close_cross_inline_svg_1 = __importDefault(
  require('../../../assets/images/close-cross.inline.svg')
);
const ClearButton_scss_1 = __importDefault(require('./ClearButton.scss'));
function ClearButton({ label, onClick }) {
  return react_1.default.createElement(
    PopOver_1.PopOver,
    { content: label, placement: 'top' },
    react_1.default.createElement(
      'button',
      {
        onClick: onClick,
        className: ClearButton_scss_1.default.component,
        tabIndex: -1,
      },
      react_1.default.createElement(react_svg_inline_1.default, {
        svg: close_cross_inline_svg_1.default,
        className: ClearButton_scss_1.default.clearIcon,
      })
    )
  );
}
exports.ClearButton = ClearButton;
//# sourceMappingURL=ClearButton.js.map

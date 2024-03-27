'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.MonospaceTextBlock = void 0;
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const MonospaceTextBlock_scss_1 = __importDefault(
  require('./MonospaceTextBlock.scss')
);
function MonospaceTextBlock({ children, className }) {
  return react_1.default.createElement(
    'pre',
    {
      className: (0, classnames_1.default)(
        MonospaceTextBlock_scss_1.default.pre,
        className
      ),
    },
    children
  );
}
exports.MonospaceTextBlock = MonospaceTextBlock;
//# sourceMappingURL=MonospaceTextBlock.js.map

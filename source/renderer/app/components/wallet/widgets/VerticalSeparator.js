'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.VerticalSeparator = void 0;
const react_1 = __importDefault(require('react'));
const Divider_scss_1 = __importDefault(require('./Divider.scss'));
function VerticalSeparator() {
  return react_1.default.createElement('span', {
    className: Divider_scss_1.default.component,
  });
}
exports.VerticalSeparator = VerticalSeparator;
//# sourceMappingURL=VerticalSeparator.js.map

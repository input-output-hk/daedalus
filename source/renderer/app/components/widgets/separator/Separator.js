'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.Separator = void 0;
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const Separator_scss_1 = __importDefault(require('./Separator.scss'));
function Separator({ className }) {
  return react_1.default.createElement('hr', {
    className: (0, classnames_1.default)(
      Separator_scss_1.default.hr,
      className
    ),
  });
}
exports.Separator = Separator;
//# sourceMappingURL=Separator.js.map

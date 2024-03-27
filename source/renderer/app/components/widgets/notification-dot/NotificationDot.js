'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const classnames_1 = __importDefault(require('classnames'));
const NotificationDot_scss_1 = __importDefault(
  require('./NotificationDot.scss')
);
function NotificationDot({
  children,
  className,
  dotClassName,
  enabled = false,
}) {
  return react_1.default.createElement(
    'div',
    {
      className: (0, classnames_1.default)(
        NotificationDot_scss_1.default.root,
        className
      ),
    },
    enabled &&
      react_1.default.createElement('span', {
        className: (0, classnames_1.default)(
          NotificationDot_scss_1.default.dot,
          dotClassName
        ),
      }),
    children
  );
}
exports.default = NotificationDot;
//# sourceMappingURL=NotificationDot.js.map

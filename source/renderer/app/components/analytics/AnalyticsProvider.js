'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.AnalyticsProvider = void 0;
const react_1 = __importDefault(require('react'));
const AnalyticsContext_1 = require('./AnalyticsContext');
function AnalyticsProvider({ children, tracker }) {
  return react_1.default.createElement(
    AnalyticsContext_1.AnalyticsContext.Provider,
    { value: tracker },
    children
  );
}
exports.AnalyticsProvider = AnalyticsProvider;
//# sourceMappingURL=AnalyticsProvider.js.map

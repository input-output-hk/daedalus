'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.noopAnalyticsTracker = exports.EventCategories = exports.AnalyticsAcceptanceStatus = void 0;
var types_1 = require('./types');
Object.defineProperty(exports, 'AnalyticsAcceptanceStatus', {
  enumerable: true,
  get: function () {
    return types_1.AnalyticsAcceptanceStatus;
  },
});
Object.defineProperty(exports, 'EventCategories', {
  enumerable: true,
  get: function () {
    return types_1.EventCategories;
  },
});
var NoopAnalyticsTracker_1 = require('./NoopAnalyticsTracker');
Object.defineProperty(exports, 'noopAnalyticsTracker', {
  enumerable: true,
  get: function () {
    return NoopAnalyticsTracker_1.noopAnalyticsTracker;
  },
});
//# sourceMappingURL=index.js.map

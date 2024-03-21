'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.noopAnalyticsTracker = void 0;
class NoopAnalyticsTracker {
  async enableTracking() {}
  disableTracking() {}
  sendPageNavigationEvent(pageTitle) {}
  sendEvent(category, name) {}
}
exports.noopAnalyticsTracker = new NoopAnalyticsTracker();
//# sourceMappingURL=NoopAnalyticsTracker.js.map

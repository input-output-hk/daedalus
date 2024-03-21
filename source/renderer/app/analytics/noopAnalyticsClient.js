'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.NoopAnalyticsClient = void 0;
const NoopAnalyticsClient = {
  sendPageNavigationEvent: () => Promise.resolve(),
  sendEvent: () => Promise.resolve(),
};
exports.NoopAnalyticsClient = NoopAnalyticsClient;
//# sourceMappingURL=noopAnalyticsClient.js.map

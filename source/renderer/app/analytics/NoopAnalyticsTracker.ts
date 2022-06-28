export const NoopAnalyticsTracker = {
  async enableTracking() {},
  disableTracking() {},
  sendPageNavigationEvent(pageTitle: string) {},
  sendEvent(category: string, name: string) {},
};

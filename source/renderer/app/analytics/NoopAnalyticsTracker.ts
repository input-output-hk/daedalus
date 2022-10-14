import { AnalyticsTracker } from '.';

class NoopAnalyticsTracker implements AnalyticsTracker {
  async enableTracking() {}
  disableTracking() {}
  sendPageNavigationEvent(pageTitle: string) {}
  sendEvent(category: string, name: string) {}
}

export const noopAnalyticsTracker = new NoopAnalyticsTracker();

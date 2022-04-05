import { AnalyticsClient } from './types';

const NoopAnalyticsClient: AnalyticsClient = {
  sendPageNavigationEvent: () => Promise.resolve(),
  sendEvent: () => Promise.resolve(),
};

export { NoopAnalyticsClient };

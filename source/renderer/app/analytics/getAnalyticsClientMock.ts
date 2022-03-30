import { AnalyticsClient } from './types';

const getAnalyticsClientMock = (): AnalyticsClient => ({
  sendPageNavigationEvent: () => Promise.resolve(),
  sendEvent: () => Promise.resolve(),
});

export { getAnalyticsClientMock };

import { AnalyticsClient } from './types';

const getAnalyticsClientMock = (): AnalyticsClient => ({
  sendPageNavigationEvent: () => Promise.resolve(),
});

export { getAnalyticsClientMock };

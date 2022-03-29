import { AnalyticsClient } from './types';

const getAnalyticsClientMock = (): AnalyticsClient => ({
  sendMachineSpec: () => Promise.resolve(),
  sendPageNavigationEvent: () => Promise.resolve(),
});

export { getAnalyticsClientMock };

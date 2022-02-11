import { AnalyticsClient } from './types';

const getAnalyticsClientMock = (): AnalyticsClient => ({
  sendMachineSpec: () => Promise.resolve(),
});

export { getAnalyticsClientMock };

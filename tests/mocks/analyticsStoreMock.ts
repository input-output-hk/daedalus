import {NoopAnalyticsClient} from "../../source/renderer/app/analytics";

export const analyticsStoreMock = {
  analyticsClient: NoopAnalyticsClient,
  setup: jest.fn(),
  resetAnalyticsClient: jest.fn()
};

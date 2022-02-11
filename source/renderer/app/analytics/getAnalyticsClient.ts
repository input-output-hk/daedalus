import { GOOGLE_ANALYTICS_TRACKING_ID } from '../config/analyticsConfig';
import { GoogleAnalytics } from './GoogleAnalyticsClient';
import { Environment } from '../../../common/types/environment.types';
import { AnalyticsClient } from './types';
import { getAnalyticsClientMock } from './getAnalyticsClientMock';
import LocalStorageApi from '../api/utils/localStorage';

let client: AnalyticsClient;

const getAnalyticsClient = async (
  localStorage: LocalStorageApi,
  environment: Environment
) => {
  if (!client) {
    client = environment.analyticsEnabled
      ? new GoogleAnalytics(
          GOOGLE_ANALYTICS_TRACKING_ID,
          environment.network,
          await localStorage.getUserID()
        )
      : getAnalyticsClientMock();
  }

  return client;
};

export { getAnalyticsClient };

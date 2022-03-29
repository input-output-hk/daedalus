import { Environment } from '../../../common/types/environment.types';
import { AnalyticsClient } from './types';
import { getAnalyticsClientMock } from './getAnalyticsClientMock';
import LocalStorageApi from '../api/utils/localStorage';
import { MatomoClient } from './MatomoClient';

let client: AnalyticsClient;

const getAnalyticsClient = async (
  localStorage: LocalStorageApi,
  environment: Environment
) => {
  if (!client) {
    client = environment.analyticsEnabled
      ? new MatomoClient(environment, await localStorage.getUserID())
      : getAnalyticsClientMock();
  }

  return client;
};

export { getAnalyticsClient };

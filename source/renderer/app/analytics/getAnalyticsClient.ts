import { Environment } from '../../../common/types/environment.types';
import { AnalyticsAcceptanceStatus, AnalyticsClient } from './types';
import { NoopAnalyticsClient } from './noopAnalyticsClient';
import LocalStorageApi from '../api/utils/localStorage';
import { MatomoClient } from './MatomoClient';

let client: AnalyticsClient;

const getAnalyticsClient = async (
  localStorage: LocalStorageApi,
  environment: Environment
) => {
  const analyticsAccepted =
    (await localStorage.getAnalyticsAcceptance()) ===
    AnalyticsAcceptanceStatus.ACCEPTED;

  if (environment.analyticsFeatureEnabled && analyticsAccepted) {
    if (!client) {
      client = new MatomoClient(environment, await localStorage.getUserID());
    }
    return client;
  }

  return NoopAnalyticsClient;
};

export { getAnalyticsClient };

import { Environment } from '../../../common/types/environment.types';
import { NoopAnalyticsClient } from './noopAnalyticsClient';
import LocalStorageApi from '../api/utils/localStorage';

const getAnalyticsClient = async (_: LocalStorageApi, __: Environment) => {
  // Matomo Client will be implemented in a separate PR
  return NoopAnalyticsClient;
};

export { getAnalyticsClient };

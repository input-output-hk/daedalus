import { AnalyticsClient } from '../analytics';
import LocalStorageApi from '../api/utils/localStorage';
import { Environment } from '../../../common/types/environment.types';
import { logger } from '../utils/logging';
import { AnalyticsAcceptanceStatus } from '../analytics/types';

export const runSendMachineSpecAnalyticsJob = async (
  analytics: AnalyticsClient,
  localStorage: LocalStorageApi
) => {
  const analyticsAccepted =
    (await localStorage.getAnalyticsAcceptance()) ===
    AnalyticsAcceptanceStatus.ACCEPTED;
  const machineSpecSent = await localStorage.getAnalyticsMachineSpecSent();

  if (!analyticsAccepted || machineSpecSent) return;

  try {
    await analytics.sendMachineSpec();
    await localStorage.setAnalyticsMachineSpecSent();
  } catch (error) {
    logger.error('AnalyticsClient::sendMachineSpec error', { error });
  }
};

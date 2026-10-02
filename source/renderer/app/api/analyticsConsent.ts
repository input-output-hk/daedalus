import { analyticsConsent } from '../ipc/ariadneAnalytics';
import {
  ARIADNE_CONSENT_VERSION,
  ConsentStatus,
  ConsentView,
} from '../../../common/analytics/contract';

export default class AnalyticsConsentApi {
  get = (): Promise<ConsentView> => analyticsConsent({ get: true });
  set = async (status: ConsentStatus): Promise<ConsentView> => {
    const view = await analyticsConsent({
      version: ARIADNE_CONSENT_VERSION,
      status,
    });
    if (
      !view ||
      view.status !== status ||
      (status === 'ACCEPTED' && !view.enabled)
    )
      throw new Error('Analytics choice could not be saved');
    return view;
  };
}

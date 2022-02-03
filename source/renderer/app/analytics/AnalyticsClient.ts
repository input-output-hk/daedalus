import { GOOGLE_ANALYTICS_TRACKING_ID } from '../config/analyticsConfig';
import { GoogleAnalytics } from './GoogleAnalyticsClient';

const AnalyticsClient = new GoogleAnalytics(
  GOOGLE_ANALYTICS_TRACKING_ID,
  global.environment.network,
  // TODO
  // daedalus.api.localStorage.getUserID()
  'UID_test'
);

export { AnalyticsClient };

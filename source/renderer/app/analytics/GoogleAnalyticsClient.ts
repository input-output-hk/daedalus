import { AnalyticsClient } from './types';

class GoogleAnalytics implements AnalyticsClient {
  async sendPageNavigationEvent() {
    throw new Error('Not implemented');
  }

  async sendEvent() {
    throw new Error('Not implemented');
  }
}

export { GoogleAnalytics };

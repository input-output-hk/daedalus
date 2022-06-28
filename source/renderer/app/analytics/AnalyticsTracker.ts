import { AnalyticsAcceptanceStatus } from '.';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import LocalStorageApi from '../api/utils/localStorage';
import { MatomoClient } from './MatomoClient';
import { NoopAnalyticsClient } from './noopAnalyticsClient';

export class AnalyticsTracker {
  analyticsClient: AnalyticsClient;

  constructor(
    private environment: Environment,
    private localStorage: LocalStorageApi
  ) {
    this.analyticsClient = NoopAnalyticsClient;
    this.#enableTrackingIfAccepted();
  }

  async enableTracking() {
    this.analyticsClient = new MatomoClient(
      this.environment,
      await localStorage.getUserID()
    );
  }

  disableTracking() {
    this.analyticsClient = NoopAnalyticsClient;
  }

  sendPageNavigationEvent(pageTitle: string) {
    return this.analyticsClient.sendPageNavigationEvent(pageTitle);
  }

  sendEvent(category: string, name: string) {
    return this.analyticsClient.sendEvent(category, name);
  }

  async #enableTrackingIfAccepted() {
    const analyticsAccepted =
      (await this.localStorage.getAnalyticsAcceptance()) ===
      AnalyticsAcceptanceStatus.ACCEPTED;

    if (this.environment.analyticsFeatureEnabled && analyticsAccepted) {
      this.enableTracking();
    }
  }
}

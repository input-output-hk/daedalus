import { AnalyticsAcceptanceStatus, AnalyticsTracker } from '.';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import LocalStorageApi from '../api/utils/localStorage';
import { MatomoClient } from './MatomoClient';
import { NoopAnalyticsClient } from './noopAnalyticsClient';
import AdaApi from '../api/api';

export class MatomoAnalyticsTracker implements AnalyticsTracker {
  #analyticsClient: AnalyticsClient;

  constructor(
    private environment: Environment,
    private localStorageApi: LocalStorageApi,
    private adaApi: AdaApi
  ) {
    this.#analyticsClient = NoopAnalyticsClient;
    this.#enableTrackingIfAccepted();
  }

  async enableTracking() {
    this.#analyticsClient = new MatomoClient(
      this.environment,
      this.adaApi,
      await this.localStorageApi.getUserID()
    );
  }

  disableTracking() {
    this.#analyticsClient = NoopAnalyticsClient;
  }

  sendPageNavigationEvent(pageTitle: string) {
    return this.#analyticsClient.sendPageNavigationEvent(pageTitle);
  }

  sendEvent(category: string, name: string, action?: string, value?: number) {
    return this.#analyticsClient.sendEvent(category, name, action, value);
  }

  async #enableTrackingIfAccepted() {
    const analyticsAccepted =
      (await this.localStorageApi.getAnalyticsAcceptance()) ===
      AnalyticsAcceptanceStatus.ACCEPTED;

    if (this.environment.analyticsFeatureEnabled && analyticsAccepted) {
      this.enableTracking();
    }
  }
}

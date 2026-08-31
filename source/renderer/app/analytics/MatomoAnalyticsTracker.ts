import { AnalyticsAcceptanceStatus, AnalyticsTracker } from '.';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import LocalStorageApi from '../api/utils/localStorage';
import { MatomoClient } from './MatomoClient';
import { NoopAnalyticsClient } from './noopAnalyticsClient';
import AdaApi from '../api/api';
import { logger } from '../utils/logging';
import { redactLogText } from '../../../common/utils/logging';

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
    return this.#analyticsClient
      .sendPageNavigationEvent(redactLogText(pageTitle))
      .catch(() => {
        logger.warn('MatomoAnalyticsTracker: page navigation event failed');
      });
  }

  sendEvent(category: string, name: string, action?: string, value?: number) {
    return this.#analyticsClient
      .sendEvent(
        redactLogText(category),
        redactLogText(name),
        action === undefined ? undefined : redactLogText(action),
        value
      )
      .catch(() => {
        logger.warn('MatomoAnalyticsTracker: analytics event failed');
      });
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

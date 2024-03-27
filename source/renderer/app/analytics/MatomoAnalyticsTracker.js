'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.MatomoAnalyticsTracker = void 0;
const _1 = require('.');
const MatomoClient_1 = require('./MatomoClient');
const noopAnalyticsClient_1 = require('./noopAnalyticsClient');
class MatomoAnalyticsTracker {
  environment;
  localStorageApi;
  adaApi;
  #analyticsClient;
  constructor(environment, localStorageApi, adaApi) {
    this.environment = environment;
    this.localStorageApi = localStorageApi;
    this.adaApi = adaApi;
    this.#analyticsClient = noopAnalyticsClient_1.NoopAnalyticsClient;
    this.#enableTrackingIfAccepted();
  }
  async enableTracking() {
    this.#analyticsClient = new MatomoClient_1.MatomoClient(
      this.environment,
      this.adaApi,
      await this.localStorageApi.getUserID()
    );
  }
  disableTracking() {
    this.#analyticsClient = noopAnalyticsClient_1.NoopAnalyticsClient;
  }
  sendPageNavigationEvent(pageTitle) {
    return this.#analyticsClient.sendPageNavigationEvent(pageTitle);
  }
  sendEvent(category, name, action) {
    return this.#analyticsClient.sendEvent(category, name, action);
  }
  async #enableTrackingIfAccepted() {
    const analyticsAccepted =
      (await this.localStorageApi.getAnalyticsAcceptance()) ===
      _1.AnalyticsAcceptanceStatus.ACCEPTED;
    if (this.environment.analyticsFeatureEnabled && analyticsAccepted) {
      this.enableTracking();
    }
  }
}
exports.MatomoAnalyticsTracker = MatomoAnalyticsTracker;
//# sourceMappingURL=MatomoAnalyticsTracker.js.map

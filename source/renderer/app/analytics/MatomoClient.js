'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.MatomoClient = void 0;
const matomo_tracker_1 = __importDefault(require('matomo-tracker'));
const analyticsConfig_1 = require('../config/analyticsConfig');
const getCustomDimensions_1 = require('./getCustomDimensions');
/**
 * Matomo API reference:
 * https://developer.matomo.org/api-reference/tracking-api
 */
class MatomoClient {
  environment;
  adaApi;
  userId;
  matomoTracker;
  constructor(environment, adaApi, userId) {
    this.environment = environment;
    this.adaApi = adaApi;
    this.userId = userId;
    this.matomoTracker = new matomo_tracker_1.default(
      this.getMatomoSiteId(environment),
      analyticsConfig_1.ANALYTICS_API_ENDPOINT
    );
  }
  sendPageNavigationEvent = async (pageTitle) => {
    const customDimensions = await (0,
    getCustomDimensions_1.getCustomDimensions)(this.environment, this.adaApi);
    this.matomoTracker.track({
      _id: this.userId,
      action_name: pageTitle,
      url: this.getAnalyticsURL(),
      ...customDimensions,
    });
  };
  sendEvent = async (category, action, name) => {
    this.matomoTracker.track({
      _id: this.userId,
      ca: 1,
      e_c: category,
      e_a: action,
      e_n: name,
      url: this.getAnalyticsURL(),
    });
  };
  getAnalyticsURL() {
    return `http://daedalus/${window.location.hash.replace('#/', '')}`;
  }
  getMatomoSiteId(environment) {
    if (environment.isDev) {
      return analyticsConfig_1.DEV_MODE_SITE_MAP_ID;
    }
    return analyticsConfig_1.NETWORK_TO_ANALYTICS_SITE_ID_MAP[
      environment.network
    ];
  }
}
exports.MatomoClient = MatomoClient;
//# sourceMappingURL=MatomoClient.js.map

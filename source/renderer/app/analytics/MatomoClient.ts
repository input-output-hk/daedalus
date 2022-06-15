import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import formatCpuInfo from '../utils/formatCpuInfo';
import {
  ANALYTICS_API_ENDPOINT,
  DEV_MODE_SITE_MAP_ID,
  NETWORK_TO_ANALYTICS_SITE_ID_MAP,
} from '../config/analyticsConfig';

const CPU_DIMENSION_KEY = 'dimension2';
const RAM_DIMENSION_KEY = 'dimension3';
const OS_DIMENSION_KEY = 'dimension4';
const VERSION_DIMENSION_KEY = 'dimension5';

/**
 * Matomo API reference:
 * https://developer.matomo.org/api-reference/tracking-api
 */
export class MatomoClient implements AnalyticsClient {
  private matomoTracker: MatomoTracker;

  constructor(private environment: Environment, private userId: string) {
    this.matomoTracker = new MatomoTracker(
      this.getMatomoSiteId(environment),
      ANALYTICS_API_ENDPOINT
    );
  }

  sendPageNavigationEvent = async (pageTitle: string) => {
    this.matomoTracker.track({
      _id: this.userId,
      action_name: pageTitle,
      url: this.getAnalyticsURL(),
      [CPU_DIMENSION_KEY]: formatCpuInfo(this.environment.cpu),
      [RAM_DIMENSION_KEY]: Math.round(
        this.environment.ram / 1024 / 1024 / 1024
      ).toString(),
      [OS_DIMENSION_KEY]: this.environment.os,
      [VERSION_DIMENSION_KEY]: this.environment.version,
    });
  };

  sendEvent = async (
    category: string,
    action: string,
    name?: string
  ): Promise<void> => {
    this.matomoTracker.track({
      _id: this.userId,
      ca: 1,
      e_c: category,
      e_a: action,
      e_n: name,
      url: this.getAnalyticsURL(),
    });
  };

  private getAnalyticsURL() {
    return 'http://daedalus/' + window.location.hash.replace('#/', '');
  }

  private getMatomoSiteId(environment: Environment) {
    if (environment.isDev) {
      return DEV_MODE_SITE_MAP_ID;
    }

    return NETWORK_TO_ANALYTICS_SITE_ID_MAP[environment.network];
  }
}

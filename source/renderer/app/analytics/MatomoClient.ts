import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import formatCpuInfo from '../utils/formatCpuInfo';

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
      this.environment.analyticsSiteId,
      this.environment.analyticsApiEndpoint
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

  sendEvent = async (category: string, action: string): Promise<void> => {
    this.matomoTracker.track({
      _id: this.userId,
      ca: 1,
      e_c: category,
      e_a: action,
      url: this.getAnalyticsURL(),
    });
  };

  private getAnalyticsURL() {
    return 'http://daedalus/' + window.location.hash.replace('#/', '');
  }
}

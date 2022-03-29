import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient } from './types';
import { Environment } from '../../../common/types/environment.types';
import formatCpuInfo from '../utils/formatCpuInfo';

enum Dimension {
  CPU = 'dimension2',
  RAM = 'dimension3',
  OS = 'dimension4',
  Version = 'dimension5',
}

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
      [Dimension.CPU]: formatCpuInfo(this.environment.cpu),
      [Dimension.RAM]: Math.round(
        this.environment.ram / 1024 / 1024 / 1024
      ).toString(),
      [Dimension.OS]: this.environment.os,
      [Dimension.Version]: this.environment.version,
    });
  };

  private getAnalyticsURL() {
    return 'http://daedalus/' + window.location.hash.replace('#/', '');
  }
}

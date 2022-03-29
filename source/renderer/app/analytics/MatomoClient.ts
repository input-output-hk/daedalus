import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient } from './types';
import { routeTitles } from '../route-titles';
import { Environment } from '../../../common/types/environment.types';
import formatCpuInfo from '../utils/formatCpuInfo';

// TODO: move to proper separate config / parametrize by build type
const SITE_ID = 3;
const API_ENDPOINT = 'https://mazurek.matomo.cloud/matomo.php';

/**
 * Matomo API reference:
 * https://developer.matomo.org/api-reference/tracking-api
 */
export class MatomoClient implements AnalyticsClient {
  private matomoTracker: MatomoTracker;

  constructor(private environment: Environment, private userId: string) {
    this.matomoTracker = new MatomoTracker(SITE_ID, API_ENDPOINT);
  }

  sendMachineSpec = async () => {
    // Disabled for now to check if we need the logic around ensuring we send hardware spec only once
    // this.matomoTracker.track({
    //   _id: this.userId,
    //   // Not required by Matomo Tracking API, however enforced by the matomo-tracker lib.
    //   // In fully fledged implementation we might want to try to rewrite those calls as vanilla
    //   // HTTP requests (or contribute to matomo-tracker) to avoid sending 'fake' unnecessary data.
    //   url: 'http://daedalus.electron/',
    //   dimension1: this.network,
    //   dimension2: payload.cpuModel,
    //   dimension3: Math.round(payload.ramBytes / 1024 / 1024 / 1024).toString(),
    //   dimension4: payload.os,
    //   dimension5: this.version,
    // });
  };

  sendPageNavigationEvent = async (pageTitle: string, route: string) => {
    this.matomoTracker.track({
      _id: this.userId,
      action_name: pageTitle,
      url: route,
      dimension1: this.environment.network,
      dimension2: formatCpuInfo(this.environment.cpu),
      dimension3: Math.round(
        this.environment.ram / 1024 / 1024 / 1024
      ).toString(),
      dimension4: this.environment.os,
      dimension5: this.environment.version,
    });
  };
}

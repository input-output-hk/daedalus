import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient, MachineSpecPayload } from './types';

const SITE_ID = 3;
const API_ENDPOINT = 'https://mazurek.matomo.cloud/matomo.php';

/**
 * Matomo API reference:
 * https://developer.matomo.org/api-reference/tracking-api
 */
export class MatomoClient implements AnalyticsClient {
  private matomoTracker: MatomoTracker;

  constructor(private network: string, private userId: string) {
    // TODO: this should be configurable or at least extracted to constants
    this.matomoTracker = new MatomoTracker(SITE_ID, API_ENDPOINT);
  }

  sendMachineSpec = async (payload: MachineSpecPayload) =>
    this.matomoTracker.track({
      _id: this.userId,
      // Not required by Matomo Tracking API, however enforced by the matomo-tracker lib.
      // In fully fledged implementation we might want to try to rewrite those calls as vanilla
      // HTTP requests (or contribute to matomo-tracker) to avoid sending 'fake' unnecessary data.
      url: 'http://daedalus.electron/',
      dimension1: this.network,
      dimension2: payload.cpuModel,
      dimension3: Math.round(payload.ramBytes / 1024 / 1024 / 1024).toString(),
      dimension4: payload.os,
      dimension5: payload.osArch,
    });
}

import MatomoTracker from 'matomo-tracker';
import { AnalyticsClient, MachineSpecPayload } from './types';

/**
 * Matomo API reference:
 * https://developer.matomo.org/api-reference/tracking-api
 */

export class MatomoClient implements AnalyticsClient {
  private matomoTracker: MatomoTracker;

  constructor(private network: string, private userId: string) {
    this.matomoTracker = new MatomoTracker(
      2,
      'https://mazurek.matomo.cloud/matomo.php'
    );
  }

  /**
   * This event collects machine spec.
   *
   * Important:
   * Since it is not a real `event` by the design of Google Analytics (because it will be sent just once), we are using
   * event `action` property as event `subcategory` to simplify reporting in GA web panel.
   */
  sendMachineSpec = async (payload: MachineSpecPayload) => {
    // Sending device data as visit with custom dimensions
    this.matomoTracker.track({
      _id: this.userId,
      // Not required by API, however enforced by the matomo-tracker lib.
      // In fully fledged implementation we might want to try to rewrite those calls as vanilla HTTP requests
      // to avoid sending 'fake' unnecessary data.
      url: 'http://daedalus.electron/',
      dimension1: this.network,
      dimension2: payload.cpuModel,
      dimension3: Math.round(payload.ramBytes / 1024 / 1024 / 1024).toString(),
      dimension4: payload.os,
      dimension5: payload.osArch,
    });
  };
}

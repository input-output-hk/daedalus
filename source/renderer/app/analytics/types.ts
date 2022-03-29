export interface AnalyticsClient {
  sendMachineSpec(): Promise<void>;
  sendPageNavigationEvent(pageTitle: string, route: string): Promise<void>;
}

export enum AnalyticsAcceptanceStatus {
  PENDING = 'PENDING',
  ACCEPTED = 'ACCEPTED',
  REJECTED = 'REJECTED',
}

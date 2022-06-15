export interface AnalyticsClient {
  sendPageNavigationEvent(pageTitle: string): Promise<void>;
  sendEvent(category: string, action: string, name?: string): Promise<void>;
}

export enum AnalyticsAcceptanceStatus {
  PENDING = 'PENDING',
  ACCEPTED = 'ACCEPTED',
  REJECTED = 'REJECTED',
}

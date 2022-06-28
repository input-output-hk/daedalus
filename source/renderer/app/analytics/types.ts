export interface AnalyticsClient {
  sendPageNavigationEvent(pageTitle: string): Promise<void>;
  sendEvent(category: string, action: string, name?: string): Promise<void>;
}

export enum AnalyticsAcceptanceStatus {
  PENDING = 'PENDING',
  ACCEPTED = 'ACCEPTED',
  REJECTED = 'REJECTED',
}

export interface AnalyticsTracker {
  enableTracking(): Promise<void>;
  disableTracking(): void;
  sendPageNavigationEvent(pageTitle: string): void;
  sendEvent(category: string, name: string, action?: string): void;
}

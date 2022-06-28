export interface AnalyticsTracker {
  enableTracking(): Promise<void>;
  disableTracking(): void;
  sendPageNavigationEvent(pageTitle: string): void;
  sendEvent(category: string, name: string, action?: string): void;
}

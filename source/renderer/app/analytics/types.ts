export interface AnalyticsClient {
  sendPageNavigationEvent(pageTitle: string): Promise<void>;
  sendEvent(category: string, action: string, name?: string): Promise<void>;
}

export enum AnalyticsAcceptanceStatus {
  INITIAL_DECISION_REQUIRED = 'INITIAL_DECISION_REQUIRED',
  DECISION_CHANGE_REQUESTED = 'DECISION_CHANGE_REQUESTED',
  ACCEPTED = 'ACCEPTED',
  REJECTED = 'REJECTED',
}

export interface AnalyticsTracker {
  enableTracking(): Promise<void>;
  disableTracking(): void;
  sendPageNavigationEvent(pageTitle: string): void;
  sendEvent(category: EventCategories, name: string, action?: string): void;
}

export enum EventCategories {
  WALLETS = 'Wallets',
  STAKE_POOLS = 'Stake Pools',
  SETTINGS = 'Settings',
  LAYOUT = 'Layout',
  SYSTEM_MENU = 'System Menu',
  VOTING = 'Voting',
}

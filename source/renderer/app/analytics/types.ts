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

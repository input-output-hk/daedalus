import type { FunnelName } from '../../../common/analytics/contract';

export interface CapturedAttempt {
  complete(): void;
  cancel(): void;
}
export interface AnalyticsClient {
  sendPageNavigationEvent(pageTitle: string): Promise<void>;
  sendEvent(
    category: string,
    action: string,
    name?: string,
    value?: number
  ): Promise<void>;
}

export enum AnalyticsAcceptanceStatus {
  PENDING = 'PENDING',
  ACCEPTED = 'ACCEPTED',
  REJECTED = 'REJECTED',
}

export interface AnalyticsTracker {
  beginFunnel?(flow: FunnelName): CapturedAttempt | undefined;
  enableTracking(): Promise<void>;
  disableTracking(): void;
  sendPageNavigationEvent(pageTitle: string): void;
  sendEvent(
    category: EventCategories,
    name: string,
    action?: string,
    value?: number
  ): void;
}

export enum EventCategories {
  WALLETS = 'Wallets',
  STAKE_POOLS = 'Stake Pools',
  SETTINGS = 'Settings',
  LAYOUT = 'Layout',
  SYSTEM_MENU = 'System Menu',
  VOTING = 'Voting',
}

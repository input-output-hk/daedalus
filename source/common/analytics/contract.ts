import {
  analyticsActions,
  analyticsPages,
  walletLabelActions,
} from './vocabulary';

// Provisional notice: changing its version requires a fresh decision.
export const ARIADNE_CONSENT_VERSION = 2;
export const analyticsFunnels = [
  'delegation_submit',
  'voting_registration_setup',
] as const;
export type FunnelName = (typeof analyticsFunnels)[number];
export type FunnelIntent = {
  type: 'funnel_step';
  action: FunnelName;
  attempt: number;
  stage: 'started' | 'completed' | 'cancelled';
  category?: never;
  label?: never;
};
export type ConsentStatus = 'PENDING' | 'ACCEPTED' | 'REJECTED';
export type ConsentView = {
  version: number;
  status: ConsentStatus;
  enabled: boolean;
  generation: number;
};
export type ConsentCommand =
  | { version: number; status: ConsentStatus }
  | { get: true };
export type WalletFlags = {
  uses_legacy_wallet: boolean;
  uses_hardware_wallet: boolean;
};
export type EventIntent = {
  type: 'page_view' | 'custom_event';
  action: string;
  category?: string;
  label?: 'Hardware wallet' | 'Software wallet';
};
export type EventMessage = (EventIntent | FunnelIntent) &
  WalletFlags & { generation: number; ts: string };

const categories: Record<string, keyof typeof analyticsActions> = {
  Wallets: 'WALLETS',
  'Stake Pools': 'STAKE_POOLS',
  Settings: 'SETTINGS',
  Layout: 'LAYOUT',
  'System Menu': 'SYSTEM_MENU',
  Voting: 'VOTING',
};

// Drop dynamic labels/values before IPC. Neither URL nor free text is a wire field.
export function eventIntent(
  type: EventIntent['type'],
  action: string,
  category?: string,
  label?: string
): EventIntent | null {
  if (type === 'page_view')
    return (analyticsPages as readonly string[]).includes(action)
      ? { type, action }
      : null;
  const key = Object.prototype.hasOwnProperty.call(categories, category)
    ? categories[category]
    : undefined;
  if (!key || !(analyticsActions[key] as readonly string[]).includes(action))
    return null;
  return {
    type,
    action,
    category: key,
    ...(key === 'WALLETS' &&
    walletLabelActions.includes(action) &&
    (label === 'Hardware wallet' || label === 'Software wallet')
      ? { label }
      : {}),
  };
}

export function record(value: unknown): value is Record<string, unknown> {
  return !!value && typeof value === 'object' && !Array.isArray(value);
}

export function validMessage(
  value: unknown,
  now: number
): value is EventMessage {
  if (
    !record(value) ||
    Object.keys(value).some(
      (key) =>
        ![
          'type',
          'action',
          'category',
          'label',
          'uses_legacy_wallet',
          'uses_hardware_wallet',
          'generation',
          'ts',
          ...(value.type === 'funnel_step' ? ['attempt', 'stage'] : []),
        ].includes(key)
    )
  )
    return false;
  if (
    !Number.isSafeInteger(value.generation) ||
    typeof value.uses_legacy_wallet !== 'boolean' ||
    typeof value.uses_hardware_wallet !== 'boolean' ||
    typeof value.ts !== 'string' ||
    !/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z$/.test(value.ts)
  )
    return false;
  const timestamp = Date.parse(value.ts);
  if (
    !Number.isFinite(timestamp) ||
    new Date(timestamp).toISOString() !== value.ts ||
    timestamp > now ||
    timestamp < now - 30_000 ||
    typeof value.action !== 'string'
  )
    return false;
  if (value.type === 'funnel_step')
    return (
      value.category === undefined &&
      value.label === undefined &&
      (analyticsFunnels as readonly unknown[]).includes(value.action) &&
      Number.isSafeInteger(value.attempt) &&
      Number(value.attempt) > 0 &&
      typeof value.stage === 'string' &&
      ['started', 'completed', 'cancelled'].includes(value.stage)
    );
  if (value.type === 'page_view')
    return (
      value.category === undefined &&
      value.label === undefined &&
      (analyticsPages as readonly string[]).includes(value.action)
    );
  if (
    value.type !== 'custom_event' ||
    typeof value.category !== 'string' ||
    !Object.prototype.hasOwnProperty.call(analyticsActions, value.category)
  )
    return false;
  if (
    !(analyticsActions[value.category] as readonly string[]).includes(
      value.action
    )
  )
    return false;
  return (
    value.label === undefined ||
    (value.category === 'WALLETS' &&
      walletLabelActions.includes(value.action) &&
      (value.label === 'Hardware wallet' || value.label === 'Software wallet'))
  );
}

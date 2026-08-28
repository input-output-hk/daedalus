export const COLLATERAL_PREFERENCE_SCHEMA_VERSION = 1;
export const DEFAULT_COLLATERAL_TARGET_LOVELACE = '5000000';

export type CollateralState =
  | 'checking'
  | 'ready'
  | 'not-ready'
  | 'preparing'
  | 'in-use'
  | 'will-be-spent'
  | 'charged'
  | 'stale';

export type CollateralInput = Readonly<{
  transactionId: string;
  index: number;
}>;

export type CollateralPreferenceRecord = Readonly<{
  schemaVersion: typeof COLLATERAL_PREFERENCE_SCHEMA_VERSION;
  walletId: string;
  networkGenesis: string;
  targetLovelace: string;
  preferredInputs: readonly CollateralInput[];
  generation: number;
}>;

export type CollateralPreference = CollateralPreferenceRecord &
  Readonly<{ state: CollateralState }>;

export type CollateralSnapshot = Readonly<{
  corrupt: boolean;
  preference: CollateralPreference;
}>;

export type CollateralRendererRequest =
  | Readonly<{
      type: 'snapshot' | 'prepare' | 'cancel-preparation' | 'clear' | 'repair';
    }>
  | Readonly<{ type: 'track-preparation'; transactionId: string }>;

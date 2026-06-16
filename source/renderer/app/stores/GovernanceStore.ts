import { action, observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import { governanceDRepListChannel } from '../ipc/governanceChannel';
import { logger } from '../utils/logging';
import {
  GovernanceQueryErrorType,
  DRepDirectoryEntry,
  DRepAnchorPresence,
} from '../../../common/types/governance.types';

/**
 * App-domain DRep directory entry with BigNumber voting power.
 * Rehydrated from the decimal-string IPC payload.
 */
export interface AppDRepDirectoryEntry {
  /** Bech32-encoded DRep ID. */
  drepId: string;
  /** Voting power in lovelace as BigNumber, or null if ranking unavailable. */
  votingPower: BigNumber | null;
  /** Active / Inactive. */
  status: DRepDirectoryEntry['status'];
  /** Remaining epochs until expiry (null if unknown). */
  drepActivity: DRepDirectoryEntry['drepActivity'];
  /** Anchor presence (URL + hash) from on-chain. No fetch performed in slice-1. */
  anchor: DRepAnchorPresence | null;
}

export enum GovernanceRefreshState {
  Idle = 'idle',
  Loading = 'loading',
  Refreshing = 'refreshing',
  Loaded = 'loaded',
  Failed = 'failed',
}

export interface GovernanceStoreError {
  type: string;
  message: string;
  details?: string;
}

export default class GovernanceStore extends Store {
  // ---- Observables ----

  /** O(1) DRep lookup by ID. Populated alongside drepList. */
  @observable drepIndex: Map<string, AppDRepDirectoryEntry> = new Map();

  /** Full DRep list (all DReps from the ledger state). */
  @observable drepList: AppDRepDirectoryEntry[] = [];

  /** Current refresh lifecycle state. */
  @observable refreshState: GovernanceRefreshState =
    GovernanceRefreshState.Idle;

  /** Last error, if any. */
  @observable error: GovernanceStoreError | null = null;

  /** Unix timestamp (ms) when data was last successfully fetched. */
  @observable lastFetchedAt: number | null = null;

  // ---- Computed ----

  @computed get isLoading(): boolean {
    return this.refreshState === GovernanceRefreshState.Loading;
  }

  @computed get isRefreshing(): boolean {
    return this.refreshState === GovernanceRefreshState.Refreshing;
  }

  @computed get isLoaded(): boolean {
    return this.refreshState === GovernanceRefreshState.Loaded;
  }

  @computed get hasError(): boolean {
    return this.refreshState === GovernanceRefreshState.Failed;
  }

  @computed get isEmpty(): boolean {
    return this.isLoaded && this.drepList.length === 0;
  }

  @computed get drepCount(): number {
    return this.drepList.length;
  }

  // ---- Actions ----

  /**
   * Fetch the DRep list from the main process.
   * Deduplicates in-flight requests locally.
   */
  @action
  async fetchDRepList(): Promise<void> {
    // Prevent concurrent requests
    if (
      this.refreshState === GovernanceRefreshState.Loading ||
      this.refreshState === GovernanceRefreshState.Refreshing
    ) {
      return;
    }

    const hasExistingData = this.drepList.length > 0;

    runInAction(() => {
      this.refreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.error = null;
    });

    try {
      const payload = await governanceDRepListChannel.request();

      runInAction(() => {
        const entries = this._rehydrateDReps(payload.dreps);
        this.drepList = entries;
        this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));
        this.refreshState = GovernanceRefreshState.Loaded;
        this.lastFetchedAt = payload.fetchedAt;
        this.error = null;
      });
    } catch (err) {
      logger.error('GovernanceStore: fetchDRepList failed', { error: err });
      runInAction(() => {
        this.error = this._normalizeError(err);
        this.refreshState = hasExistingData
          ? GovernanceRefreshState.Loaded
          : GovernanceRefreshState.Failed;
      });
    }
  }

  /** Public refresh trigger. Same as fetchDRepList. */
  @action
  refresh(): Promise<void> {
    return this.fetchDRepList();
  }

  // ---- Lifecycle ----

  setup(): void {
    super.setup();
  }

  // ---- Private Helpers ----

  /**
   * Rehydrate IPC decimal-string lovelace values into BigNumber instances.
   * Never pass raw JSONbig.storeAsString objects through IPC or into observable state.
   */
  private _rehydrateDReps(raw: DRepDirectoryEntry[]): AppDRepDirectoryEntry[] {
    return raw.map((entry) => ({
      drepId: entry.drepId,
      votingPower: entry.votingPower ? new BigNumber(entry.votingPower) : null,
      status: entry.status,
      drepActivity: entry.drepActivity,
      anchor: entry.anchor,
    }));
  }

  private _normalizeError(err: unknown): GovernanceStoreError {
    if (err && typeof err === 'object') {
      const objectError = err as {
        __governanceError?: unknown;
        type?: unknown;
        message?: unknown;
        details?: unknown;
        queryErrorType?: unknown;
      };

      // Path 2 (preferred): marked plain object thrown by governanceChannel.ts.
      // Plain objects survive structured clone intact, so type/message/details
      // are read directly without any JSON round-trip.
      if (objectError.__governanceError === true) {
        return {
          type:
            typeof objectError.type === 'string'
              ? objectError.type
              : GovernanceQueryErrorType.Unknown,
          message:
            typeof objectError.message === 'string'
              ? objectError.message
              : String(err),
          details:
            typeof objectError.details === 'string'
              ? objectError.details
              : undefined,
        };
      }

      if (typeof objectError.message === 'string') {
        try {
          const parsed = JSON.parse(
            objectError.message
          ) as GovernanceStoreError;
          if (parsed && typeof parsed === 'object') {
            return {
              type:
                typeof parsed.type === 'string'
                  ? parsed.type
                  : GovernanceQueryErrorType.Unknown,
              message:
                typeof parsed.message === 'string'
                  ? parsed.message
                  : objectError.message,
              details:
                typeof parsed.details === 'string' ? parsed.details : undefined,
            };
          }
        } catch (_parseError) {
          // fall through to direct object normalization
        }
      }

      if (
        typeof objectError.type === 'string' ||
        typeof objectError.queryErrorType === 'string'
      ) {
        return {
          type:
            (typeof objectError.type === 'string' && objectError.type) ||
            (typeof objectError.queryErrorType === 'string' &&
              objectError.queryErrorType) ||
            GovernanceQueryErrorType.Unknown,
          message:
            typeof objectError.message === 'string'
              ? objectError.message
              : String(err),
          details:
            typeof objectError.details === 'string'
              ? objectError.details
              : undefined,
        };
      }
    }

    if (err instanceof Error) {
      const queryErr = err as Error & {
        queryErrorType?: string;
        details?: string;
      };
      return {
        type: queryErr.queryErrorType ?? GovernanceQueryErrorType.Unknown,
        message: err.message,
        details: queryErr.details,
      };
    }
    return {
      type: GovernanceQueryErrorType.Unknown,
      message: String(err),
    };
  }
}

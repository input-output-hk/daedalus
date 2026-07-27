import { action, observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import {
  governanceDRepListChannel,
  governanceDRepStakeChannel,
} from '../ipc/governanceChannel';
import { logger } from '../utils/logging';
import {
  GovernanceQueryErrorType,
  DRepDirectoryEntry,
  DRepAnchorPresence,
} from '../../../common/types/governance.types';
import { generateCohortSeed, seededShuffle } from '../utils/seededShuffle';

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

export enum VotingPowerEnrichState {
  Idle = 'idle',
  Loading = 'loading',
  Loaded = 'loaded',
  Failed = 'failed',
}

export interface GovernanceStoreError {
  type: string;
  message: string;
  details?: string;
}

/**
 * Default-cohort rule (BMVG Simplified Phase-1 sizing): exclude the 35
 * largest DReps by voting power, then show up to the next 200 eligible
 * DReps - active with more than 6 remaining drepActivity epochs - in
 * seeded-random order.
 */
const COHORT_TOP_EXCLUSION = 35;
const COHORT_MAX_SIZE = 200;
const COHORT_MIN_REMAINING_EPOCHS = 6;

/**
 * Total, deterministic ranking: BigNumber voting power descending, null
 * powers last, drepId ascending as the tie-break. Never coerces lovelace
 * to Number.
 */
function compareByVotingPowerDesc(
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number {
  if (a.votingPower && b.votingPower) {
    const cmp = b.votingPower.comparedTo(a.votingPower);
    if (cmp !== 0) return cmp;
  } else if (a.votingPower) {
    return -1;
  } else if (b.votingPower) {
    return 1;
  }
  if (a.drepId < b.drepId) return -1;
  if (a.drepId > b.drepId) return 1;
  return 0;
}

/** Canonical, deterministic tie/canonicalization order shared by the derived views. */
function compareDRepIdAsc(
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number {
  if (a.drepId < b.drepId) return -1;
  if (a.drepId > b.drepId) return 1;
  return 0;
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

  /** Phase-2 voting-power enrichment lifecycle, independent of the list. */
  @observable votingPowerState: VotingPowerEnrichState =
    VotingPowerEnrichState.Idle;

  /** Session randomization seed; replaced only by reshuffleCohort(). */
  @observable cohortSeed: number = generateCohortSeed();

  /**
   * Favorited DRep ids from the per-device Electron local store. Always
   * replaced with a fresh Set instance on change - never mutated in place -
   * so computeds, React dep arrays and observers see a new reference.
   */
  @observable favoriteDRepIds: Set<string> = new Set();

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

  @computed get isRankingUnavailable(): boolean {
    return this.votingPowerState === VotingPowerEnrichState.Failed;
  }

  /** The default cohort only exists once Phase-2 voting power has loaded. */
  @computed get isCohortActive(): boolean {
    return (
      this.votingPowerState === VotingPowerEnrichState.Loaded &&
      this.drepList.length > 0
    );
  }

  /**
   * Default cohort: rank by voting power, drop the top 35, keep up to the
   * next 200 eligible entries, then shuffle from the session seed. The
   * shuffle input is drepId-canonicalized so display order is a pure
   * function of (membership, seed) - stable across refreshes that change
   * voting powers without changing membership.
   */
  @computed get defaultCohort(): AppDRepDirectoryEntry[] | null {
    if (!this.isCohortActive) return null;
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    const eligible = ranked
      .slice(COHORT_TOP_EXCLUSION)
      .filter(
        (entry) =>
          entry.status === 'active' &&
          entry.drepActivity != null &&
          entry.drepActivity > COHORT_MIN_REMAINING_EPOCHS
      );
    const selected = eligible.slice(0, COHORT_MAX_SIZE);
    const canonical = [...selected].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }

  /** What the directory renders: the cohort when active, else the full list. */
  @computed get displayedDRepList(): AppDRepDirectoryEntry[] {
    return this.defaultCohort ?? this.drepList;
  }

  /**
   * Ids of the 35 largest DReps by voting power. Empty until ranking has
   * loaded - the ranking-unavailable banner promises that ranking-based
   * filters are disabled in that state.
   */
  @computed get top35DRepIds(): Set<string> {
    if (this.votingPowerState !== VotingPowerEnrichState.Loaded) {
      return new Set();
    }
    const ranked = [...this.drepList].sort(compareByVotingPowerDesc);
    return new Set(
      ranked.slice(0, COHORT_TOP_EXCLUSION).map((entry) => entry.drepId)
    );
  }

  /**
   * Show-all base list: every registration (top-35, sub-floor and inactive
   * included) in the same seeded-random session order as the cohort, so
   * enabling show-all never introduces ranking bias by default.
   */
  @computed get showAllList(): AppDRepDirectoryEntry[] {
    const canonical = [...this.drepList].sort(compareDRepIdAsc);
    return seededShuffle(canonical, this.cohortSeed);
  }

  // ---- Actions ----

  /**
   * Fetch the DRep directory in two phases: registrations paint the list,
   * then the stake distribution enriches voting power. Deduplicates
   * in-flight requests locally, including the enrich window.
   */
  @action
  async fetchDRepList(): Promise<void> {
    // A re-entrant refresh during the enrich window would restart Phase 1
    // mid-merge, so the guard covers both phases.
    if (
      this.refreshState === GovernanceRefreshState.Loading ||
      this.refreshState === GovernanceRefreshState.Refreshing ||
      this.votingPowerState === VotingPowerEnrichState.Loading
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
        this.votingPowerState = VotingPowerEnrichState.Loading;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      // CLI stderr can carry query context; log only the normalized type.
      logger.error('GovernanceStore: fetchDRepList failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.error = normalized;
        this.refreshState = hasExistingData
          ? GovernanceRefreshState.Loaded
          : GovernanceRefreshState.Failed;
      });
      return;
    }

    await this._enrichVotingPower();
  }

  /**
   * Phase 2: merge the stake distribution into the painted list by DRep id.
   * Failure keeps the list and flags ranking-unavailable — never an error
   * state for the directory itself.
   */
  @action
  private async _enrichVotingPower(): Promise<void> {
    try {
      const payload = await governanceDRepStakeChannel.request();

      runInAction(() => {
        const entries = this.drepList.map((entry) => {
          const stake = payload.stakeByDRepId[entry.drepId];
          return {
            ...entry,
            votingPower: stake ? new BigNumber(stake) : null,
          };
        });
        this.drepList = entries;
        this.drepIndex = new Map(entries.map((e) => [e.drepId, e]));
        this.votingPowerState = VotingPowerEnrichState.Loaded;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      logger.error('GovernanceStore: voting power enrich failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.votingPowerState = VotingPowerEnrichState.Failed;
      });
    }
  }

  /** Public refresh trigger. Same as fetchDRepList. */
  @action
  refresh(): Promise<void> {
    return this.fetchDRepList();
  }

  /**
   * Replace the session seed to reorder the default cohort. Never triggers
   * a CLI query or IPC re-fetch - membership is recomputed from the
   * already-loaded list.
   */
  @action
  reshuffleCohort(): void {
    this.cohortSeed = generateCohortSeed();
  }

  /**
   * Loads persisted favorites. A failed or malformed read keeps the empty
   * set silently: favorites are non-critical per-device state, and logging
   * here is forbidden because the payload holds DRep ids.
   */
  @action
  async loadFavorites(): Promise<void> {
    try {
      const stored = await this.api.localStorage.getDRepFavorites();
      const ids = Array.isArray(stored)
        ? stored.filter((id): id is string => typeof id === 'string')
        : [];
      runInAction(() => {
        this.favoriteDRepIds = new Set(ids);
      });
    } catch (_error) {
      // Intentionally silent - see the method comment.
    }
  }

  /**
   * Toggles one favorite and persists the whole set. A persistence failure
   * keeps the in-memory state; the next successful write stores everything.
   * Never logged - the payload holds DRep ids.
   */
  @action
  toggleFavorite(drepId: string): void {
    const next = new Set(this.favoriteDRepIds);
    if (next.has(drepId)) {
      next.delete(drepId);
    } else {
      next.add(drepId);
    }
    this.favoriteDRepIds = next;
    this.api.localStorage.setDRepFavorites([...next]).catch(() => {
      // Intentionally silent - see the method comment.
    });
  }

  // ---- Lifecycle ----

  setup(): void {
    super.setup();
    this.loadFavorites();
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

import { action, observable, computed, reaction, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import { logger } from '../utils/logging';
import { DRepAnchorPresence } from '../../../common/types/governance.types';
import { ROUTES } from '../routes-config';

export interface AppDRepDirectoryEntry {
  drepId: string;
  votingPower: BigNumber | null;
  status: 'active' | 'inactive';
  drepActivity: number | null;
  anchor: DRepAnchorPresence | null;
  verifiedName: string | null;
  doNotList: boolean;
}

export interface AppDRepDetail extends AppDRepDirectoryEntry {
  metadata: {
    objectives: string | null;
    motivations: string | null;
    qualifications: string | null;
    paymentAddress: string | null;
    references: Array<{ type: string; label: string | null; uri: string }>;
  } | null;
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

const MAX_VERIFIED_NAME_LENGTH = 80;

function clampVerifiedName(name: string | null): string | null {
  if (name == null) return null;
  return name.length <= MAX_VERIFIED_NAME_LENGTH
    ? name
    : `${name.slice(0, MAX_VERIFIED_NAME_LENGTH - 1)}…`;
}

function normalizeEntry(
  item: any,
  currentEpoch: number | null
): AppDRepDirectoryEntry {
  return {
    drepId: item.id,
    votingPower: item.voting_power
      ? new BigNumber(item.voting_power.quantity)
      : null,
    status: item.status,
    drepActivity:
      currentEpoch != null
        ? Math.max(0, item.expiry_epoch - currentEpoch)
        : null,
    anchor: item.anchor
      ? { url: item.anchor.url, hash: item.anchor.data_hash }
      : null,
    verifiedName: item.name ? clampVerifiedName(item.name) : null,
    doNotList: item.do_not_list ?? false,
  };
}

function normalizeDetail(
  item: any,
  currentEpoch: number | null
): AppDRepDetail {
  const base = normalizeEntry(item, currentEpoch);
  const m = item.metadata;
  return {
    ...base,
    metadata: m
      ? {
          objectives: m.objectives ?? null,
          motivations: m.motivations ?? null,
          qualifications: m.qualifications ?? null,
          paymentAddress: m.payment_address ?? null,
          references: (m.references ?? []).map((r: any) => ({
            type: (r['@type'] ?? 'other').toLowerCase(),
            label: r.label ?? null,
            uri: r.uri,
          })),
        }
      : null,
  };
}

// Navigation state passed between the DRep directory and the voting
// governance form. Replaces location.state, which hash history v4 silently
// discards on every history.push call.
export interface DelegationNavState {
  from?: string;
  selectedWalletId?: string | null;
  voteType?: 'abstain' | 'no_confidence' | 'drep';
  selectedDRepId?: string;
  selectedDRepVerifiedName?: string | null;
  selectedDRepAnchorUrl?: string | null;
}

export default class GovernanceStore extends Store {
  // Suggested DReps (from /dreps/suggested — primary display on page load)
  @observable suggestedDReps: AppDRepDirectoryEntry[] = [];
  @observable refreshState: GovernanceRefreshState =
    GovernanceRefreshState.Idle;
  @observable error: GovernanceStoreError | null = null;
  @observable lastFetchedAt: number | null = null;

  // Full DRep list (from /dreps — lazy-loaded for show-all, search, favorites)
  @observable allDReps: AppDRepDirectoryEntry[] = [];
  @observable allDRepsRefreshState: GovernanceRefreshState =
    GovernanceRefreshState.Idle;
  @observable allDRepsError: GovernanceStoreError | null = null;

  @observable favoriteDRepIds: Set<string> = new Set();

  // Navigation handoff for the DRep selection flow (directory ↔ governance form).
  // Hash history v4 drops location.state on every push, so this observable is
  // the single transport for the round-trip delegation form state.
  @observable delegationNavState: DelegationNavState | null = null;

  @action
  setDelegationNavState(state: DelegationNavState | null): void {
    this.delegationNavState = state;
  }

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

  @computed get isGovernancePage(): boolean {
    return (
      this.stores.router.location.pathname.indexOf(ROUTES.GOVERNANCE.ROOT) === 0
    );
  }

  @computed get isEmpty(): boolean {
    return this.isLoaded && this.suggestedDReps.length === 0;
  }

  @action
  async fetchSuggestedDReps(count = 20): Promise<void> {
    if (
      this.refreshState === GovernanceRefreshState.Loading ||
      this.refreshState === GovernanceRefreshState.Refreshing
    ) {
      return;
    }

    const hasExistingData = this.suggestedDReps.length > 0;

    runInAction(() => {
      this.refreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.error = null;
    });

    try {
      const rawDReps = await this.api.ada.listSuggestedDReps(count);
      const currentEpoch = this.stores.networkStatus.localTip?.epoch ?? null;

      runInAction(() => {
        this.suggestedDReps = rawDReps.map((item) =>
          normalizeEntry(item, currentEpoch)
        );
        this.refreshState = GovernanceRefreshState.Loaded;
        this.lastFetchedAt = Date.now();
        this.error = null;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      logger.error('GovernanceStore: fetchSuggestedDReps failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.error = normalized;
        this.refreshState = hasExistingData
          ? GovernanceRefreshState.Loaded
          : GovernanceRefreshState.Failed;
      });
    }
  }

  @action
  async fetchAllDReps(): Promise<void> {
    if (
      this.allDRepsRefreshState === GovernanceRefreshState.Loading ||
      this.allDRepsRefreshState === GovernanceRefreshState.Refreshing
    ) {
      return;
    }

    const hasExistingData = this.allDReps.length > 0;

    runInAction(() => {
      this.allDRepsRefreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.allDRepsError = null;
    });

    try {
      const rawDReps = await this.api.ada.listDReps();
      const currentEpoch = this.stores.networkStatus.localTip?.epoch ?? null;

      runInAction(() => {
        this.allDReps = rawDReps.map((item) =>
          normalizeEntry(item, currentEpoch)
        );
        this.allDRepsRefreshState = GovernanceRefreshState.Loaded;
        this.allDRepsError = null;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      logger.error('GovernanceStore: fetchAllDReps failed', {
        errorType: normalized.type,
      });
      runInAction(() => {
        this.allDRepsError = normalized;
        this.allDRepsRefreshState = hasExistingData
          ? GovernanceRefreshState.Loaded
          : GovernanceRefreshState.Failed;
      });
    }
  }

  @action
  async loadAllDReps(): Promise<void> {
    if (this.allDReps.length > 0) return;
    return this.fetchAllDReps();
  }

  async fetchDRep(drepId: string): Promise<AppDRepDetail> {
    const rawDRep = await this.api.ada.getDRep(drepId);
    const currentEpoch = this.stores.networkStatus.localTip?.epoch ?? null;
    return normalizeDetail(rawDRep, currentEpoch);
  }

  @action
  refresh(): Promise<void> {
    return this.fetchSuggestedDReps();
  }

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
      // Intentionally silent - non-critical per-device state.
    }
  }

  @action
  toggleFavorite(drepId: string): void {
    const next = new Set(this.favoriteDRepIds);
    if (next.has(drepId)) {
      next.delete(drepId);
    } else {
      next.add(drepId);
    }
    this.favoriteDRepIds = next;
    this.api.localStorage.setDRepFavorites([...next]).catch(() => {});
  }

  @action
  private _clearGovernanceState(): void {
    this.suggestedDReps = [];
    this.allDReps = [];
    this.refreshState = GovernanceRefreshState.Idle;
    this.allDRepsRefreshState = GovernanceRefreshState.Idle;
    this.error = null;
    this.allDRepsError = null;
    this.lastFetchedAt = null;
  }

  setup(): void {
    super.setup();
    this.loadFavorites();
    reaction(
      () => this.isGovernancePage,
      (isOnPage) => {
        if (!isOnPage) this._clearGovernanceState();
      }
    );
  }

  private _normalizeError(err: unknown): GovernanceStoreError {
    if (err instanceof Error) {
      return {
        type: 'UNKNOWN',
        message: err.message,
      };
    }
    if (err && typeof err === 'object') {
      const obj = err as Record<string, unknown>;
      return {
        type: typeof obj.type === 'string' ? obj.type : 'UNKNOWN',
        message: typeof obj.message === 'string' ? obj.message : String(err),
        details: typeof obj.details === 'string' ? obj.details : undefined,
      };
    }
    return { type: 'UNKNOWN', message: String(err) };
  }
}

import { action, observable, computed, reaction, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import Store from './lib/Store';
import { logger } from '../utils/logging';
import { DRepAnchorPresence } from '../../../common/types/governance.types';
import type {
  DRepAdditionalField,
  DRepAnchorResult,
} from '../../../common/types/governance.types';
import { governanceDRepAnchorChannel } from '../ipc/governanceChannel';
import { ROUTES } from '../routes-config';
import {
  DEFAULT_DREP_COHORT_CRITERIA,
  createDRepCohortSeed,
  drawDRepCohort,
  nextDistinctDRepCohortSeed,
  selectDRepCohortPool,
} from '../components/governance/_shared/drepCohort';
import type {
  DRepCohortCriteria,
  DRepCohortPool,
} from '../components/governance/_shared/drepCohort';

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
    /**
     * Fields the document carried that no standard Daedalus knows defines.
     *
     * Only ever populated from a locally verified anchor: cardano-wallet's
     * response reports the canonical fields and nothing else, so a DRep's own
     * vocabulary reaches the page only by our having read the document.
     */
    additionalFields: DRepAdditionalField[];
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

// A DRep that published an empty string for a CIP-119 field has not provided
// that field. Treating blank as absent here keeps every consumer from having to
// decide separately, and stops the detail view rendering a labelled row with
// nothing in it. A reference without a uri is likewise nothing to link to.
function blankToNull(value: unknown): string | null {
  if (typeof value !== 'string') return null;
  const trimmed = value.trim();
  return trimmed.length > 0 ? trimmed : null;
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
          objectives: blankToNull(m.objectives),
          motivations: blankToNull(m.motivations),
          qualifications: blankToNull(m.qualifications),
          paymentAddress: blankToNull(m.payment_address),
          additionalFields: [],
          references: (m.references ?? [])
            .filter((r: any) => blankToNull(r?.uri) !== null)
            .map((r: any) => ({
              type: (r['@type'] ?? 'other').toLowerCase(),
              label: blankToNull(r.label),
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
  // Full DRep list (from /dreps). Every view of the directory is drawn from
  // it, the suggested cohort included, so it loads when the directory opens.
  @observable allDReps: AppDRepDirectoryEntry[] = [];
  @observable refreshState: GovernanceRefreshState =
    GovernanceRefreshState.Idle;
  @observable error: GovernanceStoreError | null = null;
  @observable lastFetchedAt: number | null = null;

  /**
   * What the suggested cohort is drawn from, and how it is drawn.
   *
   * Both are ours rather than the wallet backend's. The criteria are the
   * directory's stated defaults and the user can change any of them; the seed
   * decides which members of the eligible pool are shown this time, and
   * changes on every reroll.
   */
  @observable cohortCriteria: DRepCohortCriteria = DEFAULT_DREP_COHORT_CRITERIA;
  @observable cohortSeed: number = createDRepCohortSeed();

  @observable drepSummary: {
    totalDRepStake: BigNumber;
    activeDRepCount: number;
    inactiveDRepCount: number;
    totalDRepCount: number;
  } | null = null;
  // Tracked separately from refreshState: the summary is a non-critical
  // companion to the directory, but a permanently failing one must be
  // distinguishable from a summary that simply has not been asked for. A
  // wallet build predating /dreps/summary fails this on every refresh, and
  // swallowing it made the dependent badge silently absent.
  @observable drepSummaryState: GovernanceRefreshState =
    GovernanceRefreshState.Idle;

  @observable favoriteDRepIds: Set<string> = new Set();

  // Per-DRep cache for individually fetched entries (keyed by CIP-129 drepId).
  // Populated by ensureDRep; avoids re-fetching when navigating back to the
  // Wallets page after the suggested/all lists have not yet loaded.
  @observable fetchedDReps: Map<string, AppDRepDirectoryEntry> = new Map();

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
    // `location` is null until the router syncs with history, and setup()
    // registers a reaction on this computed that evaluates before that.
    const pathname = this.stores.router.location?.pathname;
    return pathname != null && pathname.indexOf(ROUTES.GOVERNANCE.ROOT) === 0;
  }

  @computed get isDRepSummaryAvailable(): boolean {
    return (
      this.drepSummaryState === GovernanceRefreshState.Loaded &&
      this.drepSummary !== null
    );
  }

  @computed get isEmpty(): boolean {
    return this.isLoaded && this.suggestedDReps.length === 0;
  }

  /**
   * The DReps the cohort may be drawn from, and the criteria that produced
   * them. Carries whichever criteria had to be given up to fill a cohort, so
   * the directory can say so rather than silently showing something other
   * than what its own controls claim.
   */
  @computed get cohortPool(): DRepCohortPool {
    return selectDRepCohortPool(
      this.allDReps,
      this.cohortCriteria,
      this.drepSummary?.totalDRepStake ?? null
    );
  }

  /**
   * The suggested cohort: what the directory offers before anyone searches or
   * widens the list. Selected here rather than by the wallet backend, so the
   * criteria are the directory's own and every one of them is adjustable.
   */
  @computed get suggestedDReps(): AppDRepDirectoryEntry[] {
    return drawDRepCohort(this.cohortPool, this.cohortSeed);
  }

  @action
  setCohortCriteria(criteria: DRepCohortCriteria): void {
    this.cohortCriteria = criteria;
  }

  /**
   * Draws a fresh cohort from the same pool.
   *
   * No request is made: the pool is already in hand, and the previous seed is
   * stepped until the cohort it draws differs from the one on screen.
   */
  @action
  rerollCohort(): void {
    const previousIds = new Set(this.suggestedDReps.map((e) => e.drepId));
    this.cohortSeed = nextDistinctDRepCohortSeed(
      this.cohortPool,
      this.cohortSeed,
      previousIds
    );
  }

  @action
  async fetchDRepSummary(): Promise<void> {
    runInAction(() => {
      this.drepSummaryState =
        this.drepSummary === null
          ? GovernanceRefreshState.Loading
          : GovernanceRefreshState.Refreshing;
    });

    try {
      const raw = await this.api.ada.getDRepSummary();
      runInAction(() => {
        this.drepSummary = {
          totalDRepStake: new BigNumber(raw.total_drep_stake.quantity),
          activeDRepCount: raw.active_drep_count,
          inactiveDRepCount: raw.inactive_drep_count,
          totalDRepCount: raw.total_drep_count,
        };
        this.drepSummaryState = GovernanceRefreshState.Loaded;
      });
    } catch (err) {
      logger.error('GovernanceStore: fetchDRepSummary failed', {
        errorType: this._normalizeError(err).type,
      });
      // Non-critical for the directory, which renders without it, but the
      // failure is recorded so dependent UI can say why it has nothing to show.
      runInAction(() => {
        this.drepSummaryState = GovernanceRefreshState.Failed;
      });
    }
  }

  @action
  async fetchAllDReps(): Promise<void> {
    if (
      this.refreshState === GovernanceRefreshState.Loading ||
      this.refreshState === GovernanceRefreshState.Refreshing
    ) {
      return;
    }

    const hasExistingData = this.allDReps.length > 0;

    runInAction(() => {
      this.refreshState = hasExistingData
        ? GovernanceRefreshState.Refreshing
        : GovernanceRefreshState.Loading;
      this.error = null;
    });

    try {
      // The summary carries the denominator the voting-power criterion needs.
      // Awaited alongside the list rather than after it, so the cohort is
      // drawn once against complete figures instead of being drawn without a
      // ceiling and then redrawn under one. It resolves either way: a failure
      // is recorded in its own state, and an unknown share is not excluded.
      const [rawDReps] = await Promise.all([
        this.api.ada.listDReps(),
        this.fetchDRepSummary(),
      ]);
      const currentEpoch = this.stores.networkStatus.localTip?.epoch ?? null;

      runInAction(() => {
        this.allDReps = rawDReps.map((item) =>
          normalizeEntry(item, currentEpoch)
        );
        this.refreshState = GovernanceRefreshState.Loaded;
        this.lastFetchedAt = Date.now();
        this.error = null;
      });
    } catch (err) {
      const normalized = this._normalizeError(err);
      logger.error('GovernanceStore: fetchAllDReps failed', {
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
  async loadAllDReps(): Promise<void> {
    if (this.allDReps.length > 0) return;
    return this.fetchAllDReps();
  }

  /**
   * Anchor content verified here rather than upstream.
   *
   * The main process fetches the document, checks it against the on-chain
   * hash, and keeps what verified in a content-addressed cache on disk. The
   * key is the hash itself, so an entry cannot go stale: a DRep that
   * republishes changes the hash on chain, which is a different key, and the
   * old document is simply never asked for again.
   *
   * Held per hash rather than per DRep ID for the same reason, and mirrored in
   * memory so one directory pass does not ask the main process the same
   * question once per card.
   */
  @observable anchorResults: Map<string, DRepAnchorResult> = new Map();

  async resolveAnchor(
    anchor: DRepAnchorPresence
  ): Promise<DRepAnchorResult | null> {
    const known = this.anchorResults.get(anchor.hash);
    if (known) return known;
    try {
      const result = await governanceDRepAnchorChannel.request(anchor);
      runInAction(() => {
        this.anchorResults.set(anchor.hash, result);
      });
      return result;
    } catch (error) {
      logger.warn('GovernanceStore: anchor resolution failed', {
        error: (error as Error)?.message,
      });
      return null;
    }
  }

  async fetchDRep(drepId: string): Promise<AppDRepDetail> {
    const rawDRep = await this.api.ada.getDRep(drepId);
    const currentEpoch = this.stores.networkStatus.localTip?.epoch ?? null;
    const detail = normalizeDetail(rawDRep, currentEpoch);
    if (detail.anchor == null) return detail;

    // Our own copy leads, and the wallet's is the fallback.
    //
    // Both are gated on the same on-chain hash, so where both exist they are
    // byte-identical: there is no question of which is more correct, only of
    // who does the work and how often. Ours is fetched once per hash ever and
    // then read from disk, while the wallet's is whatever it managed at the
    // moment it was asked, so a host that was briefly unreachable reads there
    // exactly like a DRep that never published.
    //
    // The wallet still covers what our fetcher will not touch. It accepts
    // https alone, so an ipfs:// anchor resolves to UnsupportedScheme here and
    // falls through to whatever the wallet made of it.
    const resolved = await this.resolveAnchor(detail.anchor);
    if (resolved?.status !== 'verified') return detail;

    const { content } = resolved;
    return {
      ...detail,
      verifiedName: blankToNull(content.givenName) ?? detail.verifiedName,
      doNotList: content.doNotList || detail.doNotList,
      metadata: {
        objectives: content.objectives ?? detail.metadata?.objectives ?? null,
        motivations:
          content.motivations ?? detail.metadata?.motivations ?? null,
        qualifications:
          content.qualifications ?? detail.metadata?.qualifications ?? null,
        paymentAddress:
          content.paymentAddress ?? detail.metadata?.paymentAddress ?? null,
        references: content.references.length
          ? content.references.map((reference) => ({
              type: reference.type,
              label: reference.label,
              uri: reference.uri,
            }))
          : (detail.metadata?.references ?? []),
        // Only our own read produces these. The wallet reports the canonical
        // fields and nothing else, so there is nothing to fall back to.
        additionalFields: content.additionalFields,
      },
    };
  }

  /**
   * Favourited DReps resolved to entries, in the order they were favourited.
   *
   * The default directory shows a random cohort of twenty, so a favourite is
   * usually not in it. Resolving them separately is what lets the directory pin
   * them above the cohort rather than leaving them reachable only by switching
   * to show-all and toggling a filter. Ids that have not resolved yet are
   * skipped rather than rendered half-built.
   */
  @computed get favoriteEntries(): AppDRepDirectoryEntry[] {
    return Array.from(this.favoriteDRepIds)
      .map((drepId) => this.lookupDRep(drepId))
      .filter((entry): entry is AppDRepDirectoryEntry => entry !== null);
  }

  /** Fetches any favourite the caches cannot already answer for. */
  async ensureFavorites(): Promise<void> {
    await Promise.all(
      Array.from(this.favoriteDRepIds).map((drepId) => this.ensureDRep(drepId))
    );
  }

  lookupDRep(drepId: string): AppDRepDirectoryEntry | null {
    return (
      this.allDReps.find((e) => e.drepId === drepId) ??
      this.fetchedDReps.get(drepId) ??
      null
    );
  }

  @action
  async ensureDRep(drepId: string): Promise<void> {
    if (this.lookupDRep(drepId) !== null) return;
    try {
      const entry = await this.fetchDRep(drepId);
      runInAction(() => {
        this.fetchedDReps.set(drepId, entry);
      });
    } catch {
      // Non-critical; the "loading" caption degrades gracefully.
    }
  }

  @action
  refresh(): Promise<void> {
    return this.fetchAllDReps();
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
    // The cohort follows from the list, so clearing the list clears it. The
    // criteria survive: they are the user's settings for this session, not
    // fetched data, and coming back to the directory should not undo them.
    this.allDReps = [];
    this.fetchedDReps = new Map();
    this.drepSummary = null;
    this.drepSummaryState = GovernanceRefreshState.Idle;
    this.refreshState = GovernanceRefreshState.Idle;
    this.error = null;
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

import type BigNumber from 'bignumber.js';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import { isLapsingSoon } from './drepExpiry';
import { hasVerifiedMetadata } from './drepMetadata';
import {
  HIGH_VOTING_POWER_THRESHOLD,
  getVotingPowerShare,
} from './drepVotingPower';

/**
 * The suggested cohort: which DReps the directory offers first, and in what
 * order.
 *
 * The cohort is drawn here, in the renderer, from the full DRep list rather
 * than from a server that decides for us. Every rule below is a default the
 * user can change from the directory, so what the wallet suggests is a
 * position it states rather than one it inherits.
 */

/** How many DReps the cohort holds unless the user asks for another number. */
export const DEFAULT_DREP_COHORT_SIZE = 20;

/** Cohort sizes the directory offers. */
export const DREP_COHORT_SIZE_OPTIONS: readonly number[] = [10, 20, 50];

/**
 * Voting-power ceilings the directory offers, as a share of total delegated
 * stake. The middle value is the threshold the high-voting-power warning
 * already uses, so the cohort and the badge cannot disagree about what counts
 * as a large DRep.
 */
export const DREP_COHORT_VOTING_POWER_SHARE_OPTIONS: readonly number[] = [
  0.005,
  HIGH_VOTING_POWER_THRESHOLD,
  0.05,
];

export interface DRepCohortCriteria {
  /** Only DReps whose registration is currently active. */
  activeOnly: boolean;
  /**
   * Exclude DReps whose voting power lapses within the next few epochs, at the
   * same threshold the status badge marks.
   */
  excludeLapsingSoon: boolean;
  /**
   * Exclusive upper bound on a DRep's share of total delegated voting power.
   * `null` means no ceiling.
   */
  maxVotingPowerShare: number | null;
  /**
   * Only DReps whose off-chain metadata was fetched and verified, so a
   * suggestion can be read as something other than an identifier.
   */
  requireVerifiedMetadata: boolean;
  /** How many DReps the cohort holds. */
  size: number;
}

export const DEFAULT_DREP_COHORT_CRITERIA: DRepCohortCriteria = {
  activeOnly: true,
  excludeLapsingSoon: true,
  maxVotingPowerShare: HIGH_VOTING_POWER_THRESHOLD,
  requireVerifiedMetadata: true,
  size: DEFAULT_DREP_COHORT_SIZE,
};

export type DRepCohortCriterion =
  | 'active'
  | 'notLapsingSoon'
  | 'votingPowerShare'
  | 'verifiedMetadata';

/**
 * The order criteria are given up in when too few DReps satisfy all of them.
 *
 * Ascending order of what the criterion protects against. A DRep with no
 * verified metadata is an information gap the card already states, so that
 * goes first. The voting-power ceiling is a stance on concentration, and a
 * card over the line is marked as such. Lapsing soon is a delegation that
 * quietly stops counting. Inactive is one that does not count now, so it is
 * the last thing given up.
 */
export const DREP_COHORT_RELAXATION_ORDER: readonly DRepCohortCriterion[] = [
  'verifiedMetadata',
  'votingPowerShare',
  'notLapsingSoon',
  'active',
];

/** How many reseeds a reroll tries before accepting a repeated cohort. */
export const MAX_COHORT_RESEED_ATTEMPTS = 8;

/**
 * A DRep that published a verified anchor asking not to be listed is never
 * suggested. That is the DRep's own instruction rather than a preference of
 * ours, so it is not among the criteria the user can relax.
 */
export function isListableDRep(entry: AppDRepDirectoryEntry): boolean {
  return !entry.doNotList;
}

export function isActiveDRep(entry: AppDRepDirectoryEntry): boolean {
  return entry.status === 'active';
}

/**
 * Whether a DRep's share of delegated stake sits under the ceiling.
 *
 * An unknown share passes. A DRep with no recorded voting power holds none of
 * it, which is below any ceiling; and when the total delegated stake has not
 * loaded the share cannot be computed at all, in which case a ceiling that
 * excluded everything would empty the directory over a companion endpoint
 * being unavailable.
 */
export function isWithinVotingPowerCap(
  entry: AppDRepDirectoryEntry,
  maxVotingPowerShare: number | null,
  totalDRepStake: BigNumber | null
): boolean {
  if (maxVotingPowerShare == null) return true;
  const share = getVotingPowerShare(entry.votingPower, totalDRepStake);
  if (share == null) return true;
  return share < maxVotingPowerShare;
}

/** Whether one DRep satisfies every criterion currently in force. */
export function isEligibleForDRepCohort(
  entry: AppDRepDirectoryEntry,
  criteria: DRepCohortCriteria,
  totalDRepStake: BigNumber | null
): boolean {
  if (!isListableDRep(entry)) return false;
  if (criteria.activeOnly && !isActiveDRep(entry)) return false;
  if (criteria.excludeLapsingSoon && isLapsingSoon(entry.drepActivity)) {
    return false;
  }
  if (
    !isWithinVotingPowerCap(entry, criteria.maxVotingPowerShare, totalDRepStake)
  ) {
    return false;
  }
  if (criteria.requireVerifiedMetadata && !hasVerifiedMetadata(entry)) {
    return false;
  }
  return true;
}

/** Whether a criterion is currently being applied at all. */
export function isDRepCohortCriterionApplied(
  criteria: DRepCohortCriteria,
  criterion: DRepCohortCriterion
): boolean {
  switch (criterion) {
    case 'active':
      return criteria.activeOnly;
    case 'notLapsingSoon':
      return criteria.excludeLapsingSoon;
    case 'votingPowerShare':
      return criteria.maxVotingPowerShare != null;
    case 'verifiedMetadata':
      return criteria.requireVerifiedMetadata;
    default:
      return false;
  }
}

function withoutCriterion(
  criteria: DRepCohortCriteria,
  criterion: DRepCohortCriterion
): DRepCohortCriteria {
  switch (criterion) {
    case 'active':
      return { ...criteria, activeOnly: false };
    case 'notLapsingSoon':
      return { ...criteria, excludeLapsingSoon: false };
    case 'votingPowerShare':
      return { ...criteria, maxVotingPowerShare: null };
    case 'verifiedMetadata':
      return { ...criteria, requireVerifiedMetadata: false };
    default:
      return criteria;
  }
}

export interface DRepCohortPool {
  /** Every DRep the cohort may be drawn from. */
  entries: AppDRepDirectoryEntry[];
  /** The criteria actually in force, after any relaxation. */
  criteria: DRepCohortCriteria;
  /** Criteria given up to reach the requested size, in the order given up. */
  relaxed: DRepCohortCriterion[];
  /** How many DReps satisfied the criteria as the user set them. */
  strictSize: number;
}

/**
 * The pool the cohort is drawn from.
 *
 * On mainnet the strict pool is roughly a fifth of registered DReps, which is
 * ample for a cohort of twenty. On a test network, early in an epoch, or with
 * the criteria tightened it can fall short, and an almost empty directory is
 * worse than a suggestion that misses one mark. So criteria are given up one
 * at a time, in a fixed order, until the pool can fill a cohort or there is
 * nothing left to give up. Criteria the user already turned off are skipped:
 * they were not applied, so relaxing them is neither a change nor something to
 * report.
 */
export function selectDRepCohortPool(
  entries: AppDRepDirectoryEntry[],
  criteria: DRepCohortCriteria,
  totalDRepStake: BigNumber | null
): DRepCohortPool {
  const listable = entries.filter(isListableDRep);
  const eligibleUnder = (applied: DRepCohortCriteria) =>
    listable.filter((entry) =>
      isEligibleForDRepCohort(entry, applied, totalDRepStake)
    );

  let applied = criteria;
  let pool = eligibleUnder(applied);
  const strictSize = pool.length;
  const relaxed: DRepCohortCriterion[] = [];

  for (const criterion of DREP_COHORT_RELAXATION_ORDER) {
    if (pool.length >= criteria.size) break;
    if (isDRepCohortCriterionApplied(applied, criterion)) {
      applied = withoutCriterion(applied, criterion);
      relaxed.push(criterion);
      pool = eligibleUnder(applied);
    }
  }

  return { entries: pool, criteria: applied, relaxed, strictSize };
}

/**
 * mulberry32, a 32-bit generator whose whole state is the seed. Two things
 * matter here: the sequence is reproducible from that seed alone, so a cohort
 * can be asserted in a test and reproduced in a bug report, and neighbouring
 * seeds produce unrelated sequences, so stepping the seed by one is a genuine
 * reshuffle rather than a nudge.
 */
export function createSeededRandom(seed: number): () => number {
  let state = seed >>> 0;
  return () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
}

/** Fisher-Yates over a copy, driven by the seeded generator alone. */
export function seededShuffle<T>(items: readonly T[], seed: number): T[] {
  const shuffled = [...items];
  const random = createSeededRandom(seed);
  for (let i = shuffled.length - 1; i > 0; i--) {
    const j = Math.floor(random() * (i + 1));
    const swapped = shuffled[i];
    shuffled[i] = shuffled[j];
    shuffled[j] = swapped;
  }
  return shuffled;
}

/** The cohort a given seed draws from a given pool. */
export function drawDRepCohort(
  pool: DRepCohortPool,
  seed: number
): AppDRepDirectoryEntry[] {
  return seededShuffle(pool.entries, seed).slice(0, pool.criteria.size);
}

export function nextDRepCohortSeed(seed: number): number {
  return (seed + 1) >>> 0;
}

/**
 * The next seed that draws a cohort the previous one did not.
 *
 * "Show different suggestions" has to show different suggestions. A fresh
 * shuffle of a large pool repeats itself only by coincidence, but coincidence
 * is not a guarantee, so the seed is stepped until at least one DRep on screen
 * is new. Where the pool is no larger than the cohort every seed yields the
 * same members and there is nothing else to show, which is why the search is
 * bounded rather than open ended.
 */
export function nextDistinctDRepCohortSeed(
  pool: DRepCohortPool,
  seed: number,
  previousIds: ReadonlySet<string>
): number {
  let candidate = seed;
  for (let attempt = 0; attempt < MAX_COHORT_RESEED_ATTEMPTS; attempt++) {
    candidate = nextDRepCohortSeed(candidate);
    const drawn = drawDRepCohort(pool, candidate);
    if (drawn.some((entry) => !previousIds.has(entry.drepId))) {
      return candidate;
    }
  }
  return candidate;
}

/**
 * The seed a session starts from.
 *
 * Deliberately not a constant. The cohort exists to spread attention across
 * DReps that would otherwise never be seen, and every wallet opening on the
 * same twenty would defeat that as thoroughly as ranking them by stake.
 */
export function createDRepCohortSeed(): number {
  return Date.now() >>> 0;
}

/**
 * The order the full DRep list is shown in when no explicit sort is chosen.
 *
 * A raw list of a thousand DReps in whatever order the ledger returned them
 * asks a reader to assess each one from scratch. These bands answer the
 * question the reader is actually holding, which is what delegating to this
 * DRep would achieve, and they answer it with facts the entry already carries
 * rather than with an opinion about the person behind it.
 *
 * Order within a band is randomised, because any stable order inside a band
 * privileges whoever sits at the top of it, and that is the popularity bias the
 * cohort is drawn randomly to avoid. The one exception is the concentrated
 * band, which is sorted by voting power ascending: there the figure is exactly
 * what separates its members, and the least concentrated of them is the one a
 * reader should meet first.
 */
export enum DRepStandingBand {
  /** Everything the cohort criteria ask for. */
  Suggestible = 0,
  /** As above, but its voting power lapses soon unless it records activity. */
  LapsingSoon = 1,
  /** Active and accountable, but already holds a large share of governance. */
  Concentrated = 2,
  /** Active, but has published nothing that verifies against its anchor. */
  Unaccountable = 3,
  /**
   * Last, behind even a DRep that published nothing.
   *
   * An inactive DRep's voting power does not count, so delegating to one
   * discards the delegation rather than merely spending it poorly.
   */
  Inactive = 4,
}

export function getDRepStandingBand(
  entry: AppDRepDirectoryEntry,
  totalDRepStake: BigNumber | null
): DRepStandingBand {
  if (entry.status !== 'active') return DRepStandingBand.Inactive;
  if (!hasVerifiedMetadata(entry)) return DRepStandingBand.Unaccountable;
  if (
    !isWithinVotingPowerCap(entry, HIGH_VOTING_POWER_THRESHOLD, totalDRepStake)
  ) {
    return DRepStandingBand.Concentrated;
  }
  if (isLapsingSoon(entry.drepActivity)) return DRepStandingBand.LapsingSoon;
  return DRepStandingBand.Suggestible;
}

const compareVotingPowerAscending = (
  a: AppDRepDirectoryEntry,
  b: AppDRepDirectoryEntry
): number => {
  if (a.votingPower && b.votingPower)
    return a.votingPower.comparedTo(b.votingPower);
  if (a.votingPower) return -1;
  if (b.votingPower) return 1;
  return 0;
};

export function orderDRepsByStanding(
  entries: AppDRepDirectoryEntry[],
  totalDRepStake: BigNumber | null,
  seed: number
): AppDRepDirectoryEntry[] {
  const bands = new Map<DRepStandingBand, AppDRepDirectoryEntry[]>();
  entries.forEach((entry) => {
    const band = getDRepStandingBand(entry, totalDRepStake);
    const members = bands.get(band);
    if (members) members.push(entry);
    else bands.set(band, [entry]);
  });

  const ordered: DRepStandingBand[] = [
    DRepStandingBand.Suggestible,
    DRepStandingBand.LapsingSoon,
    DRepStandingBand.Concentrated,
    DRepStandingBand.Unaccountable,
    DRepStandingBand.Inactive,
  ];

  const result: AppDRepDirectoryEntry[] = [];
  ordered.forEach((band, index) => {
    const members = bands.get(band) ?? [];
    if (band === DRepStandingBand.Concentrated) {
      result.push(...[...members].sort(compareVotingPowerAscending));
      return;
    }
    // A distinct seed per band, so two bands of the same length do not receive
    // the same permutation of their own members.
    result.push(...seededShuffle(members, seed + index));
  });
  return result;
}

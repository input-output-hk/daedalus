import BigNumber from 'bignumber.js';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import {
  DEFAULT_DREP_COHORT_CRITERIA,
  DEFAULT_DREP_COHORT_SIZE,
  DREP_COHORT_RELAXATION_ORDER,
  DRepStandingBand,
  createDRepCohortSeed,
  createSeededRandom,
  drawDRepCohort,
  getDRepStandingBand,
  orderDRepsByStanding,
  isEligibleForDRepCohort,
  isWithinVotingPowerCap,
  nextDRepCohortSeed,
  nextDistinctDRepCohortSeed,
  seededShuffle,
  selectDRepCohortPool,
} from './drepCohort';
import { HIGH_VOTING_POWER_THRESHOLD } from './drepVotingPower';
import { LAPSING_SOON_EPOCHS } from './drepExpiry';

// A round denominator keeps every share in these tests exact: one unit of
// voting power below is one hundredth of a percent.
const TOTAL_DREP_STAKE = new BigNumber(1_000_000);

/** Voting power holding the given share of the total above. */
const powerForShare = (share: number) => TOTAL_DREP_STAKE.multipliedBy(share);

const entryAt = (
  i: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  drepId: `drep1cohort${String(i).padStart(4, '0')}`,
  votingPower: powerForShare(0.001),
  status: 'active',
  drepActivity: 20,
  anchor: null,
  verifiedName: `DRep ${i}`,
  doNotList: false,
  ...overrides,
});

const entriesAt = (
  count: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
) => Array.from({ length: count }, (_, i) => entryAt(i, overrides));

const idsOf = (entries: AppDRepDirectoryEntry[]) =>
  entries.map((entry) => entry.drepId);

describe('isEligibleForDRepCohort', () => {
  const isEligible = (
    overrides: Partial<AppDRepDirectoryEntry>,
    criteria = DEFAULT_DREP_COHORT_CRITERIA
  ) =>
    isEligibleForDRepCohort(entryAt(0, overrides), criteria, TOTAL_DREP_STAKE);

  it('accepts a DRep meeting every default criterion', () => {
    expect(isEligible({})).toBe(true);
  });

  it('rejects an inactive DRep, and accepts it once the criterion is off', () => {
    expect(isEligible({ status: 'inactive' })).toBe(false);
    expect(
      isEligible(
        { status: 'inactive' },
        { ...DEFAULT_DREP_COHORT_CRITERIA, activeOnly: false }
      )
    ).toBe(true);
  });

  it('rejects a DRep lapsing at or within the threshold', () => {
    expect(isEligible({ drepActivity: LAPSING_SOON_EPOCHS })).toBe(false);
    expect(isEligible({ drepActivity: 0 })).toBe(false);
    expect(isEligible({ drepActivity: LAPSING_SOON_EPOCHS + 1 })).toBe(true);
    expect(
      isEligible(
        { drepActivity: 0 },
        { ...DEFAULT_DREP_COHORT_CRITERIA, excludeLapsingSoon: false }
      )
    ).toBe(true);
  });

  it('rejects a DRep at or above the voting power ceiling', () => {
    // The ceiling is exclusive: a DRep exactly on the line is the thing the
    // threshold exists to mark, so it is not suggested.
    expect(
      isEligible({ votingPower: powerForShare(HIGH_VOTING_POWER_THRESHOLD) })
    ).toBe(false);
    expect(
      isEligible({
        votingPower: powerForShare(HIGH_VOTING_POWER_THRESHOLD - 0.0001),
      })
    ).toBe(true);
    expect(
      isEligible(
        { votingPower: powerForShare(0.4) },
        { ...DEFAULT_DREP_COHORT_CRITERIA, maxVotingPowerShare: null }
      )
    ).toBe(true);
  });

  it('rejects a DRep without verified metadata', () => {
    expect(isEligible({ verifiedName: null })).toBe(false);
    expect(
      isEligible(
        { verifiedName: null },
        { ...DEFAULT_DREP_COHORT_CRITERIA, requireVerifiedMetadata: false }
      )
    ).toBe(true);
  });

  it('never suggests a DRep that asked not to be listed', () => {
    // Not one of the criteria: it is the DRep's own instruction, so turning
    // every criterion off still leaves it out.
    const everythingOff = {
      activeOnly: false,
      excludeLapsingSoon: false,
      maxVotingPowerShare: null,
      requireVerifiedMetadata: false,
      size: DEFAULT_DREP_COHORT_SIZE,
    };
    expect(isEligible({ doNotList: true })).toBe(false);
    expect(isEligible({ doNotList: true }, everythingOff)).toBe(false);
  });
});

describe('isWithinVotingPowerCap', () => {
  it('passes a DRep whose share cannot be computed', () => {
    // A share needs both numerator and denominator. Excluding what cannot be
    // measured would empty the directory whenever the summary endpoint is
    // unavailable, which is exactly when it is least helpful to do so.
    expect(
      isWithinVotingPowerCap(
        entryAt(0, { votingPower: null }),
        0.015,
        TOTAL_DREP_STAKE
      )
    ).toBe(true);
    expect(
      isWithinVotingPowerCap(
        entryAt(0, { votingPower: powerForShare(0.4) }),
        0.015,
        null
      )
    ).toBe(true);
  });

  it('passes everything when there is no ceiling', () => {
    expect(
      isWithinVotingPowerCap(
        entryAt(0, { votingPower: powerForShare(0.9) }),
        null,
        TOTAL_DREP_STAKE
      )
    ).toBe(true);
  });
});

describe('seededShuffle', () => {
  const items = Array.from({ length: 50 }, (_, i) => i);

  it('is deterministic for a given seed', () => {
    expect(seededShuffle(items, 1234)).toEqual(seededShuffle(items, 1234));
  });

  it('produces a different order for a neighbouring seed', () => {
    // Stepping the seed by one has to be a genuine reshuffle, because that is
    // exactly what a reroll does.
    expect(seededShuffle(items, 1234)).not.toEqual(seededShuffle(items, 1235));
  });

  it('is a permutation: same members, none lost or duplicated', () => {
    const shuffled = seededShuffle(items, 99);
    expect(shuffled).toHaveLength(items.length);
    expect([...shuffled].sort((a, b) => a - b)).toEqual(items);
  });

  it('leaves the input untouched', () => {
    const original = [...items];
    seededShuffle(items, 7);
    expect(items).toEqual(original);
  });

  it('handles empty and single-element inputs', () => {
    expect(seededShuffle([], 1)).toEqual([]);
    expect(seededShuffle(['only'], 1)).toEqual(['only']);
  });

  it('draws from the whole range rather than a corner of it', () => {
    const random = createSeededRandom(2024);
    const draws = Array.from({ length: 500 }, () => random());
    expect(Math.min(...draws)).toBeGreaterThanOrEqual(0);
    expect(Math.max(...draws)).toBeLessThan(1);
    const mean = draws.reduce((sum, n) => sum + n, 0) / draws.length;
    expect(mean).toBeGreaterThan(0.4);
    expect(mean).toBeLessThan(0.6);
  });
});

describe('selectDRepCohortPool', () => {
  it('keeps only eligible DReps when the pool is large enough', () => {
    const entries = [
      ...entriesAt(20),
      entryAt(100, { status: 'inactive' }),
      entryAt(101, { verifiedName: null }),
      entryAt(102, { drepActivity: 1 }),
      entryAt(103, { votingPower: powerForShare(0.2) }),
    ];
    const pool = selectDRepCohortPool(
      entries,
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );

    expect(pool.entries).toHaveLength(20);
    expect(pool.relaxed).toEqual([]);
    expect(pool.strictSize).toBe(20);
    expect(pool.criteria).toEqual(DEFAULT_DREP_COHORT_CRITERIA);
  });

  it('relaxes one criterion at a time, in order, only as far as needed', () => {
    // Nineteen ineligible only for want of metadata: dropping that criterion
    // alone fills the cohort, so nothing further is given up.
    const entries = [entryAt(0), ...entriesAt(19, { verifiedName: null })];
    const pool = selectDRepCohortPool(
      entries,
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );

    expect(pool.strictSize).toBe(1);
    expect(pool.relaxed).toEqual(['verifiedMetadata']);
    expect(pool.entries).toHaveLength(20);
    expect(pool.criteria.requireVerifiedMetadata).toBe(false);
    expect(pool.criteria.activeOnly).toBe(true);
  });

  it('gives criteria up in the documented order', () => {
    // Nothing satisfies anything, so every criterion is given up and the order
    // it happens in is the whole of what is being asserted.
    const entries = entriesAt(3, {
      status: 'inactive',
      drepActivity: 1,
      verifiedName: null,
      votingPower: powerForShare(0.3),
    });
    const pool = selectDRepCohortPool(
      entries,
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );

    expect(pool.relaxed).toEqual([...DREP_COHORT_RELAXATION_ORDER]);
    expect(pool.entries).toHaveLength(3);
  });

  it('shows what there is when relaxing everything still falls short', () => {
    const entries = entriesAt(2, { status: 'inactive' });
    const pool = selectDRepCohortPool(
      entries,
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );

    expect(pool.entries).toHaveLength(2);
    expect(drawDRepCohort(pool, 1)).toHaveLength(2);
  });

  it('does not report a criterion the user had already turned off', () => {
    // Relaxing something that was not being applied is not a change, and
    // saying so in the UI would be a claim about a rule nobody set.
    const entries = entriesAt(2, { verifiedName: null });
    const pool = selectDRepCohortPool(
      entries,
      { ...DEFAULT_DREP_COHORT_CRITERIA, requireVerifiedMetadata: false },
      TOTAL_DREP_STAKE
    );

    expect(pool.relaxed).not.toContain('verifiedMetadata');
    expect(pool.strictSize).toBe(2);
  });

  it('never relaxes the do-not-list instruction', () => {
    const entries = [
      ...entriesAt(2),
      entryAt(50, { doNotList: true }),
      entryAt(51, { doNotList: true, status: 'inactive' }),
    ];
    const pool = selectDRepCohortPool(
      entries,
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );

    expect(pool.relaxed.length).toBeGreaterThan(0);
    expect(idsOf(pool.entries)).not.toContain(entryAt(50).drepId);
    expect(idsOf(pool.entries)).not.toContain(entryAt(51).drepId);
  });

  it('yields an empty pool from an empty list without looping', () => {
    const pool = selectDRepCohortPool(
      [],
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );
    expect(pool.entries).toEqual([]);
    expect(drawDRepCohort(pool, 42)).toEqual([]);
  });
});

describe('drawDRepCohort', () => {
  const pool = selectDRepCohortPool(
    entriesAt(200),
    DEFAULT_DREP_COHORT_CRITERIA,
    TOTAL_DREP_STAKE
  );

  it('holds the cohort to its configured size', () => {
    expect(drawDRepCohort(pool, 5)).toHaveLength(DEFAULT_DREP_COHORT_SIZE);

    const smaller = selectDRepCohortPool(
      entriesAt(200),
      { ...DEFAULT_DREP_COHORT_CRITERIA, size: 10 },
      TOTAL_DREP_STAKE
    );
    expect(drawDRepCohort(smaller, 5)).toHaveLength(10);
  });

  it('draws the same cohort from the same seed', () => {
    expect(idsOf(drawDRepCohort(pool, 77))).toEqual(
      idsOf(drawDRepCohort(pool, 77))
    );
  });

  it('draws only pool members, without repeats', () => {
    const drawn = idsOf(drawDRepCohort(pool, 3));
    expect(new Set(drawn).size).toBe(drawn.length);
    const poolIds = new Set(idsOf(pool.entries));
    drawn.forEach((id) => expect(poolIds.has(id)).toBe(true));
  });
});

describe('nextDistinctDRepCohortSeed', () => {
  const previousIdsFor = (
    pool: ReturnType<typeof selectDRepCohortPool>,
    seed: number
  ) => new Set(idsOf(drawDRepCohort(pool, seed)));

  it('returns a seed drawing a different cohort', () => {
    const pool = selectDRepCohortPool(
      entriesAt(200),
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );
    const seed = 1;
    const before = previousIdsFor(pool, seed);
    const next = nextDistinctDRepCohortSeed(pool, seed, before);

    expect(next).not.toBe(seed);
    const after = idsOf(drawDRepCohort(pool, next));
    expect(after.some((id) => !before.has(id))).toBe(true);
  });

  it('keeps rerolling to fresh cohorts rather than alternating between two', () => {
    const pool = selectDRepCohortPool(
      entriesAt(200),
      DEFAULT_DREP_COHORT_CRITERIA,
      TOTAL_DREP_STAKE
    );
    let seed = 1;
    const seen: string[][] = [];
    for (let i = 0; i < 5; i++) {
      const shown = idsOf(drawDRepCohort(pool, seed));
      seen.push(shown);
      seed = nextDistinctDRepCohortSeed(pool, seed, new Set(shown));
    }
    const asStrings = seen.map((cohort) => [...cohort].sort().join(','));
    expect(new Set(asStrings).size).toBe(seen.length);
  });

  it('gives up rather than looping when the pool cannot yield anything new', () => {
    // A pool no larger than the cohort has exactly one possible membership,
    // so every seed draws the same DReps and there is nothing else to show.
    const pool = selectDRepCohortPool(
      entriesAt(3),
      { ...DEFAULT_DREP_COHORT_CRITERIA, size: 3 },
      TOTAL_DREP_STAKE
    );
    const before = previousIdsFor(pool, 1);
    const next = nextDistinctDRepCohortSeed(pool, 1, before);

    expect(idsOf(drawDRepCohort(pool, next)).sort()).toEqual(
      [...before].sort()
    );
  });

  it('steps the seed by one', () => {
    expect(nextDRepCohortSeed(1)).toBe(2);
    // Unsigned wraparound rather than a negative seed at the 32-bit boundary.
    expect(nextDRepCohortSeed(0xffffffff)).toBe(0);
  });
});

describe('createDRepCohortSeed', () => {
  it('is not a constant, so wallets do not all open on the same cohort', () => {
    const now = jest.spyOn(Date, 'now');
    now.mockReturnValue(1_000);
    const first = createDRepCohortSeed();
    now.mockReturnValue(2_000);
    const second = createDRepCohortSeed();
    now.mockRestore();

    expect(first).not.toBe(second);
  });
});

describe('orderDRepsByStanding', () => {
  const TOTAL = new BigNumber('1000000000000000'); // 1B ADA in lovelace
  const under = new BigNumber('1000000000000'); // 0.1%, under the threshold
  const over = new BigNumber('50000000000000'); // 5%, over it

  const entry = (
    id: string,
    overrides: Partial<AppDRepDirectoryEntry> = {}
  ): AppDRepDirectoryEntry => ({
    drepId: id,
    status: 'active',
    drepActivity: 12,
    anchor: { url: 'https://example.org/a.json', hash: 'a'.repeat(64) },
    verifiedName: 'Named DRep',
    doNotList: false,
    votingPower: under,
    ...overrides,
  });

  const suggestible = entry('suggestible');
  const lapsing = entry('lapsing', { drepActivity: 2 });
  const concentratedLow = entry('concentrated-low', { votingPower: over });
  const concentratedHigh = entry('concentrated-high', {
    votingPower: new BigNumber('90000000000000'),
  });
  const unaccountable = entry('unaccountable', {
    anchor: null,
    verifiedName: null,
  });
  const inactive = entry('inactive', { status: 'inactive' });
  const inactiveUnnamed = entry('inactive-unnamed', {
    status: 'inactive',
    anchor: null,
    verifiedName: null,
  });

  const idsOf = (entries: AppDRepDirectoryEntry[]) =>
    entries.map((e) => e.drepId);

  it('bands every entry by what delegating to it would achieve', () => {
    expect(getDRepStandingBand(suggestible, TOTAL)).toBe(
      DRepStandingBand.Suggestible
    );
    expect(getDRepStandingBand(lapsing, TOTAL)).toBe(
      DRepStandingBand.LapsingSoon
    );
    expect(getDRepStandingBand(concentratedLow, TOTAL)).toBe(
      DRepStandingBand.Concentrated
    );
    expect(getDRepStandingBand(unaccountable, TOTAL)).toBe(
      DRepStandingBand.Unaccountable
    );
    expect(getDRepStandingBand(inactive, TOTAL)).toBe(
      DRepStandingBand.Inactive
    );
  });

  it('puts an inactive DRep last even when it published metadata', () => {
    // Its voting power does not count, so delegating discards the delegation
    // rather than merely spending it poorly.
    const ordered = orderDRepsByStanding(
      [inactive, unaccountable, suggestible],
      TOTAL,
      1
    );
    expect(idsOf(ordered)).toEqual([
      'suggestible',
      'unaccountable',
      'inactive',
    ]);
  });

  it('orders the bands suggestible, lapsing, concentrated, unaccountable, inactive', () => {
    const ordered = orderDRepsByStanding(
      [inactive, unaccountable, concentratedLow, lapsing, suggestible],
      TOTAL,
      1
    );
    expect(idsOf(ordered)).toEqual([
      'suggestible',
      'lapsing',
      'concentrated-low',
      'unaccountable',
      'inactive',
    ]);
  });

  it('sorts the concentrated band by voting power ascending', () => {
    // There the figure is what separates its members, so the least
    // concentrated is met first rather than a random one of them.
    const ordered = orderDRepsByStanding(
      [concentratedHigh, concentratedLow],
      TOTAL,
      1
    );
    expect(idsOf(ordered)).toEqual(['concentrated-low', 'concentrated-high']);
  });

  it('keeps inactive entries together whatever else is true of them', () => {
    const ordered = orderDRepsByStanding(
      [inactiveUnnamed, inactive, suggestible],
      TOTAL,
      1
    );
    expect(idsOf(ordered)[0]).toBe('suggestible');
    expect(idsOf(ordered).slice(1).sort()).toEqual([
      'inactive',
      'inactive-unnamed',
    ]);
  });

  it('randomises within a band rather than holding a stable order', () => {
    const many = Array.from({ length: 40 }, (_, i) => entry(`s-${i}`));
    const first = idsOf(orderDRepsByStanding(many, TOTAL, 1));
    const second = idsOf(orderDRepsByStanding(many, TOTAL, 2));
    expect(first).not.toEqual(second);
    expect([...first].sort()).toEqual([...second].sort());
  });

  it('returns the same order for the same seed', () => {
    const many = Array.from({ length: 40 }, (_, i) => entry(`s-${i}`));
    expect(idsOf(orderDRepsByStanding(many, TOTAL, 9))).toEqual(
      idsOf(orderDRepsByStanding(many, TOTAL, 9))
    );
  });

  it('treats an unmeasurable share as within the cap, not over it', () => {
    // No totals means no share, and a DRep is not banded as concentrated on
    // the strength of a figure nobody has.
    expect(getDRepStandingBand(concentratedHigh, null)).toBe(
      DRepStandingBand.Suggestible
    );
  });
});

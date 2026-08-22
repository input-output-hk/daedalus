/**
 * Pure helper logic with no DOM usage. The node environment keeps Buffer in
 * the same realm as Uint8Array, which the SDK's bech32 encoder brand-checks.
 *
 * @jest-environment node
 */
import BigNumber from 'bignumber.js';
import { Cardano } from '@cardano-sdk/core';
import type { AppDRepDirectoryEntry } from '../../../stores/GovernanceStore';
import {
  DEFAULT_DREP_FILTER_STATE,
  buildDRepSearchIndex,
  filterDReps,
  getDRepQueryKind,
  isDefaultFilterState,
  isStaleFavorite,
  normalizeDRepQuery,
  resolveExactDRepMatch,
  searchDRepsByIdPrefix,
  sortDReps,
} from './helpers';
import { hasVerifiedMetadata } from '../_shared/drepMetadata';
import { LAPSING_SOON_EPOCHS } from '../_shared/drepExpiry';

// Distinct-from-the-first-byte hashes: prefix queries built from one id
// must not accidentally match another fixture's id.
const credHash = (n: number) =>
  n.toString(16).padStart(2, '0').repeat(28).slice(0, 56);

// Shared-prefix hashes (leading zeros): used ONLY by the ordering test,
// where one query must match several entries.
const sharedCredHash = (n: number) => n.toString(16).padStart(56, '0');

const cip129FromHash = (hash: string): string =>
  String(
    Cardano.DRepID.cip129FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash,
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );

const cip129At = (n: number): string => cip129FromHash(credHash(n));

const cip105At = (n: number): string =>
  String(
    Cardano.DRepID.cip105FromCredential({
      type: Cardano.CredentialType.KeyHash,
      hash: credHash(n),
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
    } as any)
  );

const buildEntry = (
  n: number,
  overrides: Partial<AppDRepDirectoryEntry> = {}
): AppDRepDirectoryEntry => ({
  anchor: null,
  verifiedName: null,
  doNotList: false,
  drepActivity: 20,
  drepId: cip129At(n),
  status: 'active',
  votingPower: null,
  ...overrides,
});

describe('normalizeDRepQuery', () => {
  it('splits HRP-qualified queries and lowercases/trims input', () => {
    expect(normalizeDRepQuery('  DRep1AbCdEfGh ')).toEqual({
      data: 'abcdefgh',
      full: 'drep1abcdefgh',
      hrp: 'drep',
    });
    expect(normalizeDRepQuery('drep_script1xyz')).toEqual({
      data: 'xyz',
      full: 'drep_script1xyz',
      hrp: 'drep_script',
    });
    expect(normalizeDRepQuery('abcdefgh')).toEqual({
      data: 'abcdefgh',
      full: 'abcdefgh',
      hrp: null,
    });
  });
});

describe('getDRepQueryKind', () => {
  it('classifies empty queries', () => {
    expect(getDRepQueryKind('')).toBe('empty');
    expect(getDRepQueryKind('drep1')).toBe('empty');
  });

  it('classifies HRP-prefixed queries with fewer than 8 data chars as belowMinimum', () => {
    // 7 characters after the HRP: below the 8-character minimum.
    expect(getDRepQueryKind('drep1abcdefg')).toBe('belowMinimum');
  });

  it('classifies a single character with no HRP as belowMinimum', () => {
    expect(getDRepQueryKind('a')).toBe('belowMinimum');
  });

  it('classifies 8 post-HRP characters in a drep1 prefix as prefix', () => {
    expect(getDRepQueryKind('drep1abcdefgh')).toBe('prefix');
  });

  it('classifies a no-HRP query of 2 or more chars as a name search', () => {
    expect(getDRepQueryKind('ab')).toBe('name');
    expect(getDRepQueryKind('abcdefg')).toBe('name');
    expect(getDRepQueryKind('abcdefgh')).toBe('name');
    expect(getDRepQueryKind('q'.repeat(60))).toBe('name');
  });

  it('classifies checksum-valid full IDs of both encodings as exactValid', () => {
    expect(getDRepQueryKind(cip129At(1))).toBe('exactValid');
    expect(getDRepQueryKind(cip105At(1))).toBe('exactValid');
  });

  it('classifies full-form-shaped strings with a bad checksum as invalidFullForm', () => {
    expect(getDRepQueryKind(`drep1${'q'.repeat(51)}`)).toBe('invalidFullForm');
    // Any single-character substitution breaks a bech32 checksum.
    const valid = cip129At(1);
    const corrupted = valid.slice(0, -1) + (valid.endsWith('q') ? 'p' : 'q');
    expect(getDRepQueryKind(corrupted)).toBe('invalidFullForm');
    // 50 post-HRP characters are still a (non-matching) prefix, not full-form.
    expect(getDRepQueryKind(`drep1${'q'.repeat(50)}`)).toBe('prefix');
  });
});

describe('buildDRepSearchIndex', () => {
  it('derives the CIP-105 twin for every canonical CIP-129 id', () => {
    const index = buildDRepSearchIndex([buildEntry(1), buildEntry(2)]);

    expect(index).toHaveLength(2);
    expect(index[0].cip129).toBe(cip129At(1));
    expect(index[0].cip105).toBe(cip105At(1));
  });

  it('keeps entries with unencodable ids searchable via CIP-129', () => {
    const index = buildDRepSearchIndex([
      buildEntry(1, { drepId: 'not-a-bech32-id' }),
    ]);

    expect(index[0].cip105).toBeNull();
    expect(index[0].cip129).toBe('not-a-bech32-id');
  });
});

describe('searchDRepsByIdPrefix', () => {
  const entries = [buildEntry(1), buildEntry(2), buildEntry(3)];
  const index = buildDRepSearchIndex(entries);

  it('matches a CIP-129 prefix of at least 8 post-HRP characters', () => {
    const query = cip129At(1).slice(0, 'drep1'.length + 12);
    const result = searchDRepsByIdPrefix(index, query);

    expect(result.map((e) => e.drepId)).toContain(cip129At(1));
  });

  it('dedupes by credential: the CIP-105 form finds the same single row', () => {
    const via129 = searchDRepsByIdPrefix(
      index,
      cip129At(2).slice(0, 'drep1'.length + 20)
    );
    const via105 = searchDRepsByIdPrefix(
      index,
      cip105At(2).slice(0, 'drep1'.length + 20)
    );

    expect(via129).toHaveLength(1);
    expect(via105).toHaveLength(1);
    expect(via105[0].drepId).toBe(via129[0].drepId);
  });

  it('returns nothing for below-minimum, invalid-full-form, and empty queries', () => {
    expect(searchDRepsByIdPrefix(index, '')).toEqual([]);
    expect(searchDRepsByIdPrefix(index, 'drep1abcdefg')).toEqual([]);
    expect(searchDRepsByIdPrefix(index, `drep1${'q'.repeat(51)}`)).toEqual([]);
  });

  it('orders multi-match results deterministically by drepId ascending', () => {
    // Shared-prefix fixtures: one 8-character query matches all three.
    const sharedEntries = [3, 1, 2].map((n) =>
      buildEntry(n, { drepId: cip129FromHash(sharedCredHash(n)) })
    );
    const sharedIndex = buildDRepSearchIndex(sharedEntries);
    const query = sharedEntries[0].drepId.slice(0, 'drep1'.length + 8);
    const broad = searchDRepsByIdPrefix(sharedIndex, query);

    expect(broad).toHaveLength(3);
    expect(broad.map((e) => e.drepId)).toEqual(
      [...broad.map((e) => e.drepId)].sort()
    );
  });
});

describe('resolveExactDRepMatch', () => {
  const entry = buildEntry(1);
  const drepIndex = new Map([[entry.drepId, entry]]);

  it('resolves a canonical CIP-129 id', () => {
    expect(resolveExactDRepMatch(cip129At(1), drepIndex)).toBe(entry);
  });

  it('canonicalizes a CIP-105 form to the same entry', () => {
    expect(resolveExactDRepMatch(cip105At(1), drepIndex)).toBe(entry);
  });

  it('returns null for valid-but-unknown, invalid, and prefix inputs', () => {
    expect(resolveExactDRepMatch(cip129At(9), drepIndex)).toBeNull();
    expect(
      resolveExactDRepMatch(`drep1${'q'.repeat(51)}`, drepIndex)
    ).toBeNull();
    expect(
      resolveExactDRepMatch(cip129At(1).slice(0, 20), drepIndex)
    ).toBeNull();
  });
});

describe('filterDReps', () => {
  it('filters by status', () => {
    const entries = [
      buildEntry(1),
      buildEntry(2, { status: 'inactive', drepActivity: 0 }),
    ];

    expect(
      filterDReps(entries, {
        ...DEFAULT_DREP_FILTER_STATE,
        status: 'inactive',
      }).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('filters by verified metadata, not by anchor presence', () => {
    // An anchor is a URL and a hash recorded on chain; it says nothing about
    // whether the content behind it was retrieved or matched. Filtering on the
    // anchor filtered a DRep in as having metadata that the card then labelled
    // as having none.
    const entries = [
      buildEntry(1, { verifiedName: 'Verified DRep' }),
      buildEntry(2, {
        anchor: { hash: 'a'.repeat(64), url: 'https://example.org/2.json' },
        verifiedName: null,
      }),
      buildEntry(3),
    ];

    expect(
      filterDReps(entries, {
        ...DEFAULT_DREP_FILTER_STATE,
        metadata: 'withMetadata',
      }).map((e) => e.drepId)
    ).toEqual([cip129At(1)]);
    expect(
      filterDReps(entries, {
        ...DEFAULT_DREP_FILTER_STATE,
        metadata: 'withoutMetadata',
      }).map((e) => e.drepId)
    ).toEqual([cip129At(2), cip129At(3)]);
  });

  it('agrees with the badge on every entry it filters', () => {
    // The filter and the badge answer the same question and must not diverge:
    // whatever "with verified metadata" admits must render as verified.
    const entries = [
      buildEntry(1, { verifiedName: 'Verified DRep' }),
      buildEntry(2, {
        anchor: { hash: 'a'.repeat(64), url: 'https://example.org/2.json' },
        verifiedName: null,
      }),
      buildEntry(3),
    ];

    filterDReps(entries, {
      ...DEFAULT_DREP_FILTER_STATE,
      metadata: 'withMetadata',
    }).forEach((entry) => {
      expect(hasVerifiedMetadata(entry)).toBe(true);
    });
    filterDReps(entries, {
      ...DEFAULT_DREP_FILTER_STATE,
      metadata: 'withoutMetadata',
    }).forEach((entry) => {
      expect(hasVerifiedMetadata(entry)).toBe(false);
    });
  });

  it('detects the default filter state', () => {
    expect(isDefaultFilterState(DEFAULT_DREP_FILTER_STATE)).toBe(true);
    expect(
      isDefaultFilterState({ ...DEFAULT_DREP_FILTER_STATE, status: 'active' })
    ).toBe(false);
  });
});

describe('sortDReps', () => {
  it('returns the input untouched for the randomized default', () => {
    const entries = [buildEntry(2), buildEntry(1)];

    expect(sortDReps(entries, 'recommended')).toBe(entries);
  });

  it('orders voting power losslessly at one lovelace beyond Number precision', () => {
    // 9007199254740993 and 9007199254740992 collapse to the same float; a
    // coerced comparison would tie and mis-order via the id tie-break.
    const smaller = buildEntry(1, {
      votingPower: new BigNumber('9007199254740993'),
    });
    const larger = buildEntry(2, {
      votingPower: new BigNumber('9007199254740992'),
    });

    expect(sortDReps([larger, smaller], 'votingPowerDesc')[0].drepId).toBe(
      cip129At(1)
    );
    expect(sortDReps([smaller, larger], 'votingPowerAsc')[0].drepId).toBe(
      cip129At(2)
    );
  });

  it('sorts null voting power last in both directions and does not mutate input', () => {
    const withPower = buildEntry(1, { votingPower: new BigNumber('5') });
    const nullPower = buildEntry(2);
    const input = [nullPower, withPower];

    expect(sortDReps(input, 'votingPowerDesc').map((e) => e.drepId)).toEqual([
      cip129At(1),
      cip129At(2),
    ]);
    expect(sortDReps(input, 'votingPowerAsc').map((e) => e.drepId)).toEqual([
      cip129At(1),
      cip129At(2),
    ]);
    expect(input.map((e) => e.drepId)).toEqual([cip129At(2), cip129At(1)]);
  });

  it('sorts by soonest expiry first with null activity last', () => {
    const entries = [
      buildEntry(1, { drepActivity: 30 }),
      buildEntry(2, { drepActivity: 8 }),
      buildEntry(3, { drepActivity: null }),
    ];

    expect(sortDReps(entries, 'expiryAsc').map((e) => e.drepId)).toEqual([
      cip129At(2),
      cip129At(1),
      cip129At(3),
    ]);
  });
});

describe('isStaleFavorite', () => {
  it('is false for every current on-chain status', () => {
    expect(isStaleFavorite(buildEntry(1))).toBe(false);
    expect(
      isStaleFavorite(buildEntry(2, { drepActivity: 0, status: 'inactive' }))
    ).toBe(false);
  });

  it('is true for a verified doNotList entry at either status', () => {
    expect(isStaleFavorite(buildEntry(3, { doNotList: true }))).toBe(true);
    expect(
      isStaleFavorite(buildEntry(4, { doNotList: true, status: 'inactive' }))
    ).toBe(true);
  });
});

describe('expiry filter', () => {
  it('hides DReps at or below the lapsing threshold', () => {
    const entries = [
      buildEntry(1, { drepActivity: 20 }),
      buildEntry(2, { drepActivity: 7 }),
      buildEntry(3, { drepActivity: 6 }),
      buildEntry(4, { drepActivity: 1 }),
    ];

    expect(
      filterDReps(entries, {
        ...DEFAULT_DREP_FILTER_STATE,
        expiry: 'hideLapsingSoon',
      }).map((e) => e.drepId)
    ).toEqual([cip129At(1), cip129At(2)]);
  });

  it('uses the same threshold the badge uses', () => {
    // Six epochs, not the seven-to-twelve window the retired Threshold badge
    // used: dRepActivity is 20 epochs and an epoch is five days, so twelve
    // remaining is 60 of a DRep's 100 days.
    expect(LAPSING_SOON_EPOCHS).toBe(6);
    const atThreshold = buildEntry(1, { drepActivity: LAPSING_SOON_EPOCHS });
    expect(
      filterDReps([atThreshold], {
        ...DEFAULT_DREP_FILTER_STATE,
        expiry: 'hideLapsingSoon',
      })
    ).toHaveLength(0);
  });

  it('keeps entries whose remaining epochs are unknown', () => {
    const unknown = buildEntry(1, { drepActivity: null });
    expect(
      filterDReps([unknown], {
        ...DEFAULT_DREP_FILTER_STATE,
        expiry: 'hideLapsingSoon',
      })
    ).toHaveLength(1);
  });

  it('changes nothing while set to all', () => {
    const entries = [buildEntry(1, { drepActivity: 2 }), buildEntry(2)];
    expect(filterDReps(entries, DEFAULT_DREP_FILTER_STATE)).toHaveLength(2);
  });

  it('counts as a non-default filter state', () => {
    expect(
      isDefaultFilterState({
        ...DEFAULT_DREP_FILTER_STATE,
        expiry: 'hideLapsingSoon',
      })
    ).toBe(false);
  });
});

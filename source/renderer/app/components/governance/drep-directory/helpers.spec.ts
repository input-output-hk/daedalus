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
  EMPTY_DREP_ID_SET,
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
  it('classifies empty and below-minimum queries counting post-HRP characters', () => {
    expect(getDRepQueryKind('')).toBe('empty');
    expect(getDRepQueryKind('drep1')).toBe('empty');
    // 7 characters after the HRP: below the 8-character minimum.
    expect(getDRepQueryKind('drep1abcdefg')).toBe('belowMinimum');
    expect(getDRepQueryKind('abcdefg')).toBe('belowMinimum');
  });

  it('classifies 8 post-HRP characters as a prefix', () => {
    expect(getDRepQueryKind('drep1abcdefgh')).toBe('prefix');
    expect(getDRepQueryKind('abcdefgh')).toBe('prefix');
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
    // Without an HRP a string can never be full-form.
    expect(getDRepQueryKind('q'.repeat(60))).toBe('prefix');
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
  const context = {
    favoriteDRepIds: EMPTY_DREP_ID_SET,
    top35DRepIds: EMPTY_DREP_ID_SET,
  };

  it('filters by status', () => {
    const entries = [
      buildEntry(1),
      buildEntry(2, { status: 'inactive', drepActivity: 0 }),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, status: 'inactive' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('filters by anchor-presence metadata', () => {
    const entries = [
      buildEntry(1, {
        anchor: { hash: 'a'.repeat(64), url: 'https://example.org/1.json' },
      }),
      buildEntry(2),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, metadata: 'withMetadata' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(1)]);
    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, metadata: 'withoutMetadata' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('applies the 7-12 expiry window with strict edges', () => {
    const entries = [
      buildEntry(1, { drepActivity: 6 }),
      buildEntry(2, { drepActivity: 7 }),
      buildEntry(3, { drepActivity: 12 }),
      buildEntry(4, { drepActivity: 13 }),
      buildEntry(5, { drepActivity: null }),
    ];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, expiry: 'thresholdWindow' },
        context
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2), cip129At(3)]);
  });

  it('excludes top-35 members via the injected id set', () => {
    const entries = [buildEntry(1), buildEntry(2)];

    expect(
      filterDReps(
        entries,
        { ...DEFAULT_DREP_FILTER_STATE, excludeTop35: true },
        { ...context, top35DRepIds: new Set([cip129At(1)]) }
      ).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
  });

  it('applies the favorited predicate against an injected set and yields nothing on the empty set', () => {
    const entries = [buildEntry(1), buildEntry(2)];
    const favoritedOnly = { ...DEFAULT_DREP_FILTER_STATE, favoritedOnly: true };

    expect(
      filterDReps(entries, favoritedOnly, {
        ...context,
        favoriteDRepIds: new Set([cip129At(2)]),
      }).map((e) => e.drepId)
    ).toEqual([cip129At(2)]);
    expect(filterDReps(entries, favoritedOnly, context)).toEqual([]);
  });

  it('detects the default filter state', () => {
    expect(isDefaultFilterState(DEFAULT_DREP_FILTER_STATE)).toBe(true);
    expect(
      isDefaultFilterState({ ...DEFAULT_DREP_FILTER_STATE, excludeTop35: true })
    ).toBe(false);
  });
});

describe('sortDReps', () => {
  it('returns the input untouched for the randomized default', () => {
    const entries = [buildEntry(2), buildEntry(1)];

    expect(sortDReps(entries, 'randomized')).toBe(entries);
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
});

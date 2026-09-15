import BigNumber from 'bignumber.js';
import {
  assetMetadataSourceTipIsFresh,
  getAssetMetadataSourceIdFromUrl,
  getAssetTokenFromToken,
  getNonZeroAssetTokens,
  searchAssets,
  sortAssets,
} from './assets';
import {
  ASSET_METADATA_SERVERS_LIST,
  ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS,
} from '../config/assetsConfig';
import type { AssetMetadata, AssetToken, Token } from '../api/assets/types';

const policyId = '6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7';
const otherPolicyId =
  '2f9e3d4a1b8c7e6f5a4b3c2d1e0f9a8b7c6d5e4f3a2b1c0d9e8f7a6b';
// 'Cointest' and 'USDC' respectively.
const assetName = '436f696e74657374';
const otherAssetName = '55534443';
// A 32-byte name whose bytes are not text.
const opaqueAssetName =
  '787c09a71b2eacdc2a7644591bd32426ed996387470bc6ec9574167ccf6af8cf';

const metadata: AssetMetadata = {
  name: 'Test Coin',
  description: 'A test coin',
  ticker: 'TEST',
};

// Shaped as the wallet balance mapping builds a token: identity already present.
const walletToken = (overrides: Partial<Token> = {}): Token => ({
  policyId,
  assetName,
  assetNameASCII: 'Cointest',
  quantity: new BigNumber(42),
  uniqueId: `${policyId}${assetName}`,
  ...overrides,
});

// Shaped as the transaction mapping builds one: no uniqueId, no assetNameASCII.
const transactionToken = (overrides: Record<string, any> = {}) =>
  ({
    policyId,
    assetName,
    quantity: new BigNumber(7),
    address: 'addr_test1qq',
    ...overrides,
  }) as Token;

const lookupOf =
  (assets: Record<string, Record<string, any>>) =>
  (lookupPolicyId: string, lookupAssetName: string) =>
    assets[`${lookupPolicyId}${lookupAssetName}`];

const emptyLookup = () => undefined;

describe('getAssetTokenFromToken', () => {
  it('keeps the identity of a token whose subject has no cached row', () => {
    const merged = getAssetTokenFromToken(walletToken(), emptyLookup);
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.policyId).toEqual(policyId);
    expect(merged.assetName).toEqual(assetName);
    expect(merged.assetNameASCII).toEqual('Cointest');
    expect(merged.quantity).toEqual(new BigNumber(42));
  });

  it('leaves the registry fields undefined when the subject has no cached row', () => {
    const merged = getAssetTokenFromToken(walletToken(), emptyLookup);
    expect(merged.metadata).toBeUndefined();
    expect(merged.decimals).toBeUndefined();
    expect(merged.recommendedDecimals).toBeUndefined();
    expect(merged.fingerprint).toBeUndefined();
  });

  it('merges a fully populated token and lookup into exactly the fields both carry', () => {
    const token = walletToken({ address: 'addr_test1qq' });
    const merged = getAssetTokenFromToken(
      token,
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: `${policyId}${assetName}`,
          policyId,
          assetName,
          fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
          metadata,
          decimals: 6,
          recommendedDecimals: 6,
        },
      })
    );
    expect(merged).toEqual({
      policyId,
      assetName,
      assetNameASCII: 'Cointest',
      quantity: new BigNumber(42),
      address: 'addr_test1qq',
      uniqueId: `${policyId}${assetName}`,
      fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
      metadata,
      decimals: 6,
      recommendedDecimals: 6,
    });
  });

  it('derives the identity of a token that originated in a transaction', () => {
    const merged = getAssetTokenFromToken(transactionToken(), emptyLookup);
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.assetNameASCII).toEqual('Cointest');
    expect(merged.quantity).toEqual(new BigNumber(7));
    expect(merged.address).toEqual('addr_test1qq');
  });

  it('derives an asset name that is not text without throwing', () => {
    const merged = getAssetTokenFromToken(
      transactionToken({ assetName: opaqueAssetName }),
      emptyLookup
    );
    expect(merged.uniqueId).toEqual(`${policyId}${opaqueAssetName}`);
    expect(typeof merged.assetNameASCII).toEqual('string');
  });

  it('derives the identity of a transaction token whose subject is cached', () => {
    const merged = getAssetTokenFromToken(
      transactionToken(),
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: 'a value the merge must not read',
          fingerprint: 'asset1cvmyrfrc7lpsnjhhz9l4rzqmc6nlp4kw2xkvpa',
          metadata,
          decimals: 6,
        },
      })
    );
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.metadata).toEqual(metadata);
    expect(merged.decimals).toEqual(6);
  });

  it('never takes identity from the lookup', () => {
    const merged = getAssetTokenFromToken(
      walletToken(),
      lookupOf({
        [`${policyId}${assetName}`]: {
          uniqueId: `${otherPolicyId}${otherAssetName}`,
          policyId: otherPolicyId,
          assetName: otherAssetName,
          assetNameASCII: 'USDC',
          metadata,
        },
      })
    );
    expect(merged.uniqueId).toEqual(`${policyId}${assetName}`);
    expect(merged.policyId).toEqual(policyId);
    expect(merged.assetName).toEqual(assetName);
    expect(merged.assetNameASCII).toEqual('Cointest');
  });
});

describe('getNonZeroAssetTokens', () => {
  it('keeps every token when no subject has a cached row', () => {
    const tokens = [
      walletToken(),
      walletToken({
        assetName: otherAssetName,
        assetNameASCII: 'USDC',
        uniqueId: `${policyId}${otherAssetName}`,
        quantity: new BigNumber(1),
      }),
    ];
    const merged = getNonZeroAssetTokens(tokens, emptyLookup);
    expect(merged).toHaveLength(2);
    expect(merged.map(({ uniqueId }) => uniqueId)).toEqual([
      `${policyId}${assetName}`,
      `${policyId}${otherAssetName}`,
    ]);
  });

  it('keeps every token when only some subjects have a cached row', () => {
    const tokens = [
      walletToken(),
      walletToken({
        assetName: otherAssetName,
        assetNameASCII: 'USDC',
        uniqueId: `${policyId}${otherAssetName}`,
        quantity: new BigNumber(1),
      }),
    ];
    const merged = getNonZeroAssetTokens(
      tokens,
      lookupOf({
        [`${policyId}${otherAssetName}`]: {
          fingerprint: 'asset1zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz',
          metadata,
        },
      })
    );
    expect(merged).toHaveLength(2);
    expect(
      merged.find(({ uniqueId }) => uniqueId === `${policyId}${otherAssetName}`)
        ?.metadata
    ).toEqual(metadata);
  });

  it('orders the tokens whose subject resolved by fingerprint', () => {
    const tokens = [
      walletToken({
        assetName: otherAssetName,
        uniqueId: `${policyId}${otherAssetName}`,
      }),
      walletToken(),
    ];
    const merged = getNonZeroAssetTokens(
      tokens,
      lookupOf({
        [`${policyId}${assetName}`]: { fingerprint: 'asset1aaa' },
        [`${policyId}${otherAssetName}`]: { fingerprint: 'asset1bbb' },
      })
    );
    expect(merged.map(({ fingerprint }) => fingerprint)).toEqual([
      'asset1aaa',
      'asset1bbb',
    ]);
  });
});

describe('searchAssets', () => {
  const unresolvedRow = () =>
    getAssetTokenFromToken(
      {
        policyId: 'a'.repeat(56),
        assetName: '424f4f4b',
        uniqueId: `${'a'.repeat(56)}424f4f4b`,
        quantity: new BigNumber(1),
      } as Token,
      () => undefined
    );

  const resolvedRow = (metadata: AssetMetadata) =>
    getAssetTokenFromToken(
      {
        policyId: 'b'.repeat(56),
        assetName: '424f4f4b',
        uniqueId: `${'b'.repeat(56)}424f4f4b`,
        quantity: new BigNumber(1),
      } as Token,
      () => ({
        fingerprint: 'asset1resolvedrowfingerprint00000000000',
        metadata,
      })
    );

  it('does not match a row through a field it does not have', () => {
    const rows = [unresolvedRow()];
    // Every one of these is three letters of a value a missing field coerces
    // to: "undefined" and "[object Object]".
    expect(searchAssets('und', rows)).toEqual([]);
    expect(searchAssets('fin', rows)).toEqual([]);
    expect(searchAssets('obj', rows)).toEqual([]);
    expect(searchAssets('ect', rows)).toEqual([]);
  });

  it('matches a row through a published name, ticker or description', () => {
    const row = resolvedRow({
      name: 'Fundamental',
      description: 'A token for the undecided',
      ticker: 'FUND',
    });
    expect(searchAssets('und', [row])).toHaveLength(1);
    expect(searchAssets('FUN', [row])).toHaveLength(1);
    expect(searchAssets('undec', [row])).toHaveLength(1);
  });

  it('matches an unresolved row through its identity', () => {
    const row = unresolvedRow();
    expect(searchAssets('aaaaaa', [row])).toHaveLength(1);
    expect(searchAssets('424f4f', [row])).toHaveLength(1);
    expect(searchAssets('BOOK', [row])).toHaveLength(1);
  });

  it('matches a resolved row through its fingerprint', () => {
    const row = resolvedRow({
      name: 'Bookmark',
      description: '',
      ticker: 'BM',
    });
    expect(searchAssets('asset1resolved', [row])).toHaveLength(1);
  });

  it('returns everything for a search of fewer than three characters', () => {
    const rows = [unresolvedRow()];
    expect(searchAssets('un', rows)).toEqual(rows);
  });
});

/**
 * The comparator behind every token list. Driven as the order a list comes out
 * in rather than as the sign of a pair, because the order is what it is for.
 *
 * The row with neither a fingerprint nor metadata is the state phase 3
 * introduced: a token the wallet holds and the cache has never heard of is
 * rendered anyway, and the comparator has to place it.
 */
describe('sortAssets', () => {
  const row = (overrides: Record<string, any>): AssetToken =>
    ({
      policyId,
      assetName,
      uniqueId: `${policyId}${assetName}`,
      quantity: new BigNumber(1),
      decimals: null,
      ...overrides,
    }) as AssetToken;

  const named = row({
    fingerprint: 'asset1bbbb',
    metadata: { name: 'Beta', description: '', ticker: 'B' },
    quantity: new BigNumber(30),
  });
  const alsoNamed = row({
    fingerprint: 'asset1aaaa',
    metadata: { name: 'Alpha', description: '', ticker: 'A' },
    quantity: new BigNumber(20),
  });
  const unnamed = row({
    fingerprint: 'asset1cccc',
    metadata: null,
    quantity: new BigNumber(10),
  });
  const unresolved = row({
    fingerprint: undefined,
    metadata: null,
    quantity: new BigNumber(40),
  });

  const order = (
    sortBy: 'token' | 'fingerprint' | 'quantity',
    direction: 'asc' | 'desc'
  ) =>
    [named, unnamed, unresolved, alsoNamed]
      .slice()
      .sort(sortAssets(sortBy, direction))
      .map((asset) => asset.quantity.toNumber());

  it('puts published names first, in order, then the rest by fingerprint', () => {
    // 20 Alpha, 30 Beta, then the two without a name: the unresolved row's
    // empty fingerprint sorts ahead of asset1cccc.
    expect(order('token', 'asc')).toEqual([20, 30, 40, 10]);
  });

  it('reverses both halves without moving a named row past an unnamed one', () => {
    expect(order('token', 'desc')).toEqual([30, 20, 10, 40]);
  });

  it('orders by fingerprint, with a row that has none sorting first', () => {
    expect(order('fingerprint', 'asc')).toEqual([40, 20, 30, 10]);
  });

  it('reverses the fingerprint order, leaving a row with none last', () => {
    expect(order('fingerprint', 'desc')).toEqual([10, 30, 20, 40]);
  });

  it('orders by quantity in the denomination each row is shown in', () => {
    expect(order('quantity', 'asc')).toEqual([10, 20, 30, 40]);
    expect(order('quantity', 'desc')).toEqual([40, 30, 20, 10]);
  });

  it('compares quantities as the user sees them, not as the ledger holds them', () => {
    // 1000 raw units at six decimal places is 0.001, which is less than 2 at
    // none. Sorting the raw integers would put it last.
    const formatted = row({
      fingerprint: 'asset1dddd',
      metadata: null,
      quantity: new BigNumber(1000),
      decimals: 6,
    });
    const raw = row({
      fingerprint: 'asset1eeee',
      metadata: null,
      quantity: new BigNumber(2),
      decimals: 0,
    });
    expect(
      [raw, formatted]
        .sort(sortAssets('quantity', 'asc'))
        .map((asset) => asset.fingerprint)
    ).toEqual(['asset1dddd', 'asset1eeee']);
  });

  it('leaves the order alone for a key it does not know', () => {
    expect(order('rank' as any, 'asc')).toEqual([30, 10, 40, 20]);
  });
});

describe('getAssetMetadataSourceIdFromUrl', () => {
  it('returns the preset id for the default URL', () => {
    expect(
      getAssetMetadataSourceIdFromUrl(ASSET_METADATA_SERVERS_LIST.koios.url)
    ).toBe('koios');
  });

  it('returns the preset id for the direct literal', () => {
    expect(getAssetMetadataSourceIdFromUrl('direct')).toBe('direct');
  });

  it('returns custom for an unrelated URL', () => {
    expect(
      getAssetMetadataSourceIdFromUrl('https://koios.example.com/api/v1')
    ).toBe('custom');
  });

  // The same host with a trailing slash is a different stored string, and the
  // reduction compares strings. Custom is the honest answer rather than a
  // near-match, and the settings page shows the URL either way.
  it('returns custom for the default URL with a trailing slash', () => {
    expect(
      getAssetMetadataSourceIdFromUrl(
        `${ASSET_METADATA_SERVERS_LIST.koios.url}/`
      )
    ).toBe('custom');
  });

  it('returns custom for the empty string', () => {
    expect(getAssetMetadataSourceIdFromUrl('')).toBe('custom');
  });
});

describe('assetMetadataSourceTipIsFresh', () => {
  const local = 131545218;

  it('accepts a source at the same tip as the node', () => {
    expect(assetMetadataSourceTipIsFresh(local, local)).toBe(true);
  });

  it('accepts a source exactly at the lag bound', () => {
    expect(
      assetMetadataSourceTipIsFresh(
        local - ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS,
        local
      )
    ).toBe(true);
  });

  it('refuses a source one slot beyond the lag bound', () => {
    expect(
      assetMetadataSourceTipIsFresh(
        local - ASSET_METADATA_SOURCE_MAX_TIP_LAG_SLOTS - 1,
        local
      )
    ).toBe(false);
  });

  // A node that is still syncing is behind everything, so being ahead is not a
  // reason to refuse.
  it('accepts a source far ahead of the node', () => {
    expect(assetMetadataSourceTipIsFresh(local + 5_000_000, local)).toBe(true);
  });

  it('accepts any source while the local tip is unknown', () => {
    expect(assetMetadataSourceTipIsFresh(1, null)).toBe(true);
    expect(assetMetadataSourceTipIsFresh(1, undefined)).toBe(true);
  });
});
